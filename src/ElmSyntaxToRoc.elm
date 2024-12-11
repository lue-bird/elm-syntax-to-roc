module ElmSyntaxToRoc exposing
    ( modules, rocDeclarationsToModuleString
    , RocLocalDeclaration(..), RocExpression(..), RocPattern(..)
    )

{-| Transpiling [`elm-syntax`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/)
declarations to roc.

@docs modules, rocDeclarationsToModuleString
@docs RocLocalDeclaration, RocExpression, RocPattern

If you need more fine-grained helpers,
[open an issue](https://github.com/lue-bird/elm-syntax-format/issues/new)


-}

import Elm.Syntax.Declaration
import Elm.Syntax.Exposing
import Elm.Syntax.Expression
import Elm.Syntax.File
import Elm.Syntax.Import
import Elm.Syntax.Module
import Elm.Syntax.ModuleName
import Elm.Syntax.Node
import Elm.Syntax.Pattern
import FastDict
import FastSet
import Print exposing (Print)
import Unicode


{-| The sub-set of roc pattern syntax used in generated roc code
-}
type RocPattern
    = RocPatternIgnore
    | RocPatternInteger Int
    | RocPatternString String
    | RocPatternVariable String
    | RocPatternRecord (FastSet.Set String)
    | RocPatternAs
        { variable : String
        , pattern : RocPattern
        }
    | RocPatternListCons
        { initialElement0 : RocPattern
        , initialElement1Up : List RocPattern
        , tailVariable : Maybe String
        }
    | RocPatternListExact (List RocPattern)
    | RocPatternTagged
        { name : String
        , parameters : List RocPattern
        }
    | RocPatternTuple
        { part0 : RocPattern
        , part1 : RocPattern
        }
    | RocPatternTriple
        { part0 : RocPattern
        , part1 : RocPattern
        , part2 : RocPattern
        }


{-| The sub-set of roc expression syntax used in generated roc code
-}
type RocExpression
    = RocExpressionUnit
    | RocExpressionInteger Int
    | RocExpressionF64 Float
    | RocExpressionString String
    | RocExpressionReference
        { moduleOrigin : Maybe String
        , name : String
        }
    | RocExpressionRecordAccessFunction String
    | RocExpressionRecordAccess
        { record : RocExpression
        , field : String
        }
    | RocExpressionTuple
        { part0 : RocExpression
        , part1 : RocExpression
        }
    | RocExpressionTriple
        { part0 : RocExpression
        , part1 : RocExpression
        , part2 : RocExpression
        }
    | RocExpressionIfThenElse
        { condition : RocExpression
        , onTrue : RocExpression
        , onFalse : RocExpression
        }
    | RocExpressionList (List RocExpression)
    | RocExpressionRecord (FastDict.Dict String RocExpression)
    | RocExpressionRecordUpdate
        { originalRecordVariable : String
        , fields : FastDict.Dict String RocExpression
        }
    | RocExpressionCall
        { called : RocExpression
        , argument0 : RocExpression
        , argument1Up : List RocExpression
        }
    | RocExpressionLambda
        { parameter0 : RocPattern
        , parameter1Up : List RocPattern
        , result : RocExpression
        }
    | RocExpressionWhenIs
        { matched : RocExpression
        , case0 :
            { pattern : RocPattern
            , result : RocExpression
            }
        , case1Up :
            List
                { pattern : RocPattern
                , result : RocExpression
                }
        }
    | RocExpressionWithLocalDeclarations
        { declaration0 : RocLocalDeclaration
        , declaration1Up : List RocLocalDeclaration
        , result : RocExpression
        }


{-| The sub-set of roc local declaration syntax used in generated roc code
-}
type RocLocalDeclaration
    = RocLocalDestructuring
        { pattern : RocPattern
        , expression : RocExpression
        }
    | RocLocalDeclarationValueOrFunction
        { name : String
        , expression : RocExpression
        }


{-| How do references used in a module map to their origin module?

Contains variants, type alias names, choice type names, port names, expression declaration names
and whether `(|.)` and or `(|=)` are imported from `Parser.Advanced`.

Also contains locally declared names when available.

-}
type alias ModuleOriginLookup =
    FastDict.Dict
        ( Elm.Syntax.ModuleName.ModuleName, String )
        Elm.Syntax.ModuleName.ModuleName


{-| Calculate valid mappings of qualifications + name
to origin module based on a module's imports.

Requires all exposed names
so we can resolve `exposing (..)` and `ChoiceType(..)`.

-}
importsToModuleOriginLookup :
    FastDict.Dict
        Elm.Syntax.ModuleName.ModuleName
        { valueOrFunctionNames : FastSet.Set String
        , choiceTypesExposingVariants : FastDict.Dict String (FastSet.Set String)
        }
    -> List (Elm.Syntax.Node.Node Elm.Syntax.Import.Import)
    -> ModuleOriginLookup
importsToModuleOriginLookup moduleExposes imports =
    let
        importsNormal :
            List
                { moduleName : Elm.Syntax.ModuleName.ModuleName
                , alias : Maybe String
                , exposes : FastSet.Set String
                }
        importsNormal =
            implicitImports
                ++ (imports
                        |> List.map
                            (\(Elm.Syntax.Node.Node _ syntaxImport) ->
                                let
                                    importModuleName : Elm.Syntax.ModuleName.ModuleName
                                    importModuleName =
                                        syntaxImport.moduleName |> Elm.Syntax.Node.value
                                in
                                { moduleName = importModuleName
                                , alias =
                                    syntaxImport.moduleAlias
                                        |> Maybe.map
                                            (\(Elm.Syntax.Node.Node _ syntaxAlias) ->
                                                syntaxAlias |> String.join "."
                                            )
                                , exposes =
                                    case syntaxImport.exposingList of
                                        Nothing ->
                                            FastSet.empty

                                        Just (Elm.Syntax.Node.Node _ syntaxExposing) ->
                                            case moduleExposes |> FastDict.get importModuleName of
                                                Nothing ->
                                                    FastSet.empty

                                                Just moduleExposedNames ->
                                                    case syntaxExposing of
                                                        Elm.Syntax.Exposing.All _ ->
                                                            moduleExposedNames.choiceTypesExposingVariants
                                                                |> FastDict.foldl
                                                                    (\_ variantNames soFar -> FastSet.union variantNames soFar)
                                                                    moduleExposedNames.valueOrFunctionNames

                                                        Elm.Syntax.Exposing.Explicit exposes ->
                                                            exposes
                                                                |> List.foldl
                                                                    (\(Elm.Syntax.Node.Node _ expose) soFar ->
                                                                        case expose of
                                                                            Elm.Syntax.Exposing.InfixExpose _ ->
                                                                                soFar

                                                                            Elm.Syntax.Exposing.TypeOrAliasExpose _ ->
                                                                                soFar

                                                                            Elm.Syntax.Exposing.FunctionExpose name ->
                                                                                soFar |> FastSet.insert (name |> rocNameSanitize)

                                                                            Elm.Syntax.Exposing.TypeExpose choiceTypeExpose ->
                                                                                case choiceTypeExpose.open of
                                                                                    Nothing ->
                                                                                        soFar

                                                                                    Just _ ->
                                                                                        case
                                                                                            moduleExposedNames.choiceTypesExposingVariants
                                                                                                |> FastDict.get choiceTypeExpose.name
                                                                                        of
                                                                                            Nothing ->
                                                                                                soFar

                                                                                            Just choiceTypeDeclared ->
                                                                                                choiceTypeDeclared
                                                                    )
                                                                    FastSet.empty
                                }
                            )
                   )
                |> importsCombine
    in
    importsNormal
        |> List.foldl
            (\syntaxImport soFar ->
                let
                    importedModuleMembers : FastSet.Set String
                    importedModuleMembers =
                        case moduleExposes |> FastDict.get syntaxImport.moduleName of
                            Nothing ->
                                FastSet.empty

                            Just moduleExposedNames ->
                                moduleExposedNames.choiceTypesExposingVariants
                                    |> FastDict.foldl
                                        (\_ variantNames variantsSoFar ->
                                            FastSet.union variantNames variantsSoFar
                                        )
                                        moduleExposedNames.valueOrFunctionNames
                in
                FastDict.union
                    (FastDict.union
                        (syntaxImport.exposes
                            |> FastSet.foldl
                                (\expose dictSoFar ->
                                    dictSoFar
                                        |> FastDict.insert ( [], expose )
                                            syntaxImport.moduleName
                                )
                                FastDict.empty
                        )
                        (case syntaxImport.alias of
                            Nothing ->
                                importedModuleMembers
                                    |> FastSet.foldl
                                        (\exposeFromImportedModule dictSoFar ->
                                            dictSoFar
                                                |> FastDict.insert
                                                    ( syntaxImport.moduleName, exposeFromImportedModule )
                                                    syntaxImport.moduleName
                                        )
                                        FastDict.empty

                            Just importAlias ->
                                importedModuleMembers
                                    |> FastSet.foldl
                                        (\exposeFromImportedModule dictSoFar ->
                                            dictSoFar
                                                |> FastDict.insert
                                                    ( [ importAlias ], exposeFromImportedModule )
                                                    syntaxImport.moduleName
                                        )
                                        FastDict.empty
                        )
                    )
                    soFar
            )
            FastDict.empty


implicitImports :
    List
        { moduleName : Elm.Syntax.ModuleName.ModuleName
        , alias : Maybe String
        , exposes : FastSet.Set String
        }
implicitImports =
    [ { moduleName = [ "Basics" ]
      , alias = Nothing
      , exposes =
            FastSet.fromList
                [ "Int"
                , "Float"
                , "toFloat"
                , "round"
                , "floor"
                , "ceiling"
                , "truncate"
                , "max"
                , "min"
                , "compare"
                , "Order"
                , "LT"
                , "EQ"
                , "GT"
                , "Bool"
                , "True"
                , "False"
                , "not"
                , "xor"
                , "modBy"
                , "remainderBy"
                , "negate"
                , "abs"
                , "clamp"
                , "sqrt"
                , "logBase"
                , "e"
                , "pi"
                , "cos"
                , "sin"
                , "tan"
                , "acos"
                , "asin"
                , "atan"
                , "atan2"
                , "degrees"
                , "radians"
                , "turns"
                , "toPolar"
                , "fromPolar"
                , "isNaN"
                , "isInfinite"
                , "identity"
                , "always"
                , "Never"
                , "never"
                ]
      }
    , { moduleName = [ "List" ], alias = Nothing, exposes = FastSet.fromList [ "List" ] }
    , { moduleName = [ "Maybe" ], alias = Nothing, exposes = FastSet.fromList [ "Maybe", "Just", "Nothing" ] }
    , { moduleName = [ "Result" ], alias = Nothing, exposes = FastSet.fromList [ "Result", "Ok", "Err" ] }
    , { moduleName = [ "String" ], alias = Nothing, exposes = FastSet.fromList [ "String" ] }
    , { moduleName = [ "Char" ], alias = Nothing, exposes = FastSet.fromList [ "Char" ] }
    , { moduleName = [ "Tuple" ], alias = Nothing, exposes = FastSet.fromList [] }
    , { moduleName = [ "Debug" ], alias = Nothing, exposes = FastSet.fromList [] }
    , { moduleName = [ "Platform" ], alias = Nothing, exposes = FastSet.fromList [ "Program" ] }
    , { moduleName = [ "Platform", "Cmd" ], alias = Just "Cmd", exposes = FastSet.fromList [ "Cmd" ] }
    , { moduleName = [ "Platform", "Sub" ], alias = Just "Sub", exposes = FastSet.fromList [ "Sub" ] }
    ]


importsCombine :
    List
        { moduleName : Elm.Syntax.ModuleName.ModuleName
        , alias : Maybe String
        , exposes : FastSet.Set String
        }
    ->
        List
            { moduleName : Elm.Syntax.ModuleName.ModuleName
            , alias : Maybe String
            , exposes : FastSet.Set String
            }
importsCombine syntaxImports =
    importsCombineFrom [] syntaxImports


importsCombineFrom :
    List
        { moduleName : Elm.Syntax.ModuleName.ModuleName
        , alias : Maybe String
        , exposes : FastSet.Set String
        }
    ->
        List
            { moduleName : Elm.Syntax.ModuleName.ModuleName
            , alias : Maybe String
            , exposes : FastSet.Set String
            }
    ->
        List
            { moduleName : Elm.Syntax.ModuleName.ModuleName
            , alias : Maybe String
            , exposes : FastSet.Set String
            }
importsCombineFrom soFar syntaxImports =
    case syntaxImports of
        [] ->
            soFar

        [ onlyImport ] ->
            onlyImport :: soFar

        import0 :: import1 :: import2Up ->
            if import0.moduleName == import1.moduleName then
                importsCombineFrom soFar
                    (importsMerge import0 import1
                        :: import2Up
                    )

            else
                importsCombineFrom
                    (import0 :: soFar)
                    (import1 :: import2Up)


importsMerge :
    { moduleName : Elm.Syntax.ModuleName.ModuleName
    , alias : Maybe String
    , exposes : FastSet.Set String
    }
    ->
        { moduleName : Elm.Syntax.ModuleName.ModuleName
        , alias : Maybe String
        , exposes : FastSet.Set String
        }
    ->
        { moduleName : Elm.Syntax.ModuleName.ModuleName
        , alias : Maybe String
        , exposes : FastSet.Set String
        }
importsMerge earlier later =
    { moduleName = earlier.moduleName
    , alias =
        case earlier.alias of
            Just alias ->
                alias |> Just

            Nothing ->
                later.alias
    , exposes =
        FastSet.union earlier.exposes later.exposes
    }


stringFirstCharToLower : String -> String
stringFirstCharToLower string =
    case string |> String.uncons of
        Nothing ->
            ""

        Just ( headChar, tailString ) ->
            String.cons (Char.toLower headChar) tailString


stringFirstCharToUpper : String -> String
stringFirstCharToUpper string =
    case string |> String.uncons of
        Nothing ->
            ""

        Just ( headChar, tailString ) ->
            String.cons (Char.toUpper headChar) tailString


stringFirstCharIsUpper : String -> Bool
stringFirstCharIsUpper string =
    case string |> String.uncons of
        Nothing ->
            False

        Just ( headChar, _ ) ->
            headChar |> Char.isUpper


stringLiteral : String -> Print
stringLiteral stringContent =
    let
        singleDoubleQuotedStringContentEscaped : String
        singleDoubleQuotedStringContentEscaped =
            stringContent
                |> String.foldl
                    (\contentChar soFar ->
                        soFar ++ singleDoubleQuotedStringCharToEscaped contentChar ++ ""
                    )
                    ""
    in
    Print.exactly ("\"" ++ singleDoubleQuotedStringContentEscaped ++ "\"")


singleDoubleQuotedStringCharToEscaped : Char -> String
singleDoubleQuotedStringCharToEscaped character =
    case character of
        '"' ->
            "\\\""

        '\\' ->
            "\\\\"

        '\t' ->
            "\\t"

        '\n' ->
            "\\n"

        '$' ->
            "\\$"

        '\u{000D}' ->
            "\\r"

        otherCharacter ->
            if characterIsNotPrint otherCharacter then
                "\\(" ++ characterHex otherCharacter ++ ")"

            else
                String.fromChar otherCharacter


intToHexString : Int -> String
intToHexString int =
    -- IGNORE TCO
    if int < 16 then
        unsafeHexDigitIntToString int

    else
        intToHexString (int // 16)
            ++ unsafeHexDigitIntToString (int |> Basics.remainderBy 16)
            ++ ""


unsafeHexDigitIntToString : Int -> String
unsafeHexDigitIntToString int =
    case int of
        0 ->
            "0"

        1 ->
            "1"

        2 ->
            "2"

        3 ->
            "3"

        4 ->
            "4"

        5 ->
            "5"

        6 ->
            "6"

        7 ->
            "7"

        8 ->
            "8"

        9 ->
            "9"

        10 ->
            "A"

        11 ->
            "B"

        12 ->
            "C"

        13 ->
            "D"

        14 ->
            "E"

        -- 15
        _ ->
            "F"


characterHex : Char -> String
characterHex character =
    String.toUpper (intToHexString (Char.toCode character))


charCodeIsLower : Int -> Bool
charCodeIsLower code =
    0x61 <= code && code <= 0x7A


charCodeIsUpper : Int -> Bool
charCodeIsUpper code =
    code <= 0x5A && 0x41 <= code


charCodeIsDigit : Int -> Bool
charCodeIsDigit code =
    code <= 0x39 && 0x30 <= code


charIsLatinAlphaNumOrUnderscoreFast : Char -> Bool
charIsLatinAlphaNumOrUnderscoreFast c =
    let
        code : Int
        code =
            Char.toCode c
    in
    charCodeIsLower code
        || charCodeIsUpper code
        || charCodeIsDigit code
        || -- (c == '_')
           (code == 95)


characterIsNotPrint : Char -> Bool
characterIsNotPrint character =
    if
        -- Unicode.getCategory is very expensive so we shortcut if at all possible
        charIsLatinAlphaNumOrUnderscoreFast character
            || (case character of
                    ' ' ->
                        True

                    '.' ->
                        True

                    '!' ->
                        True

                    '?' ->
                        True

                    '-' ->
                        True

                    ':' ->
                        True

                    _ ->
                        False
               )
    then
        False

    else
        case Unicode.getCategory character of
            Nothing ->
                True

            Just category ->
                case category of
                    Unicode.SeparatorLine ->
                        True

                    Unicode.SeparatorParagraph ->
                        True

                    Unicode.OtherControl ->
                        True

                    Unicode.OtherFormat ->
                        True

                    Unicode.OtherSurrogate ->
                        True

                    Unicode.OtherPrivateUse ->
                        True

                    Unicode.OtherNotAssigned ->
                        True

                    Unicode.LetterUppercase ->
                        False

                    Unicode.LetterLowercase ->
                        False

                    Unicode.LetterTitlecase ->
                        False

                    Unicode.MarkNonSpacing ->
                        False

                    Unicode.MarkSpacingCombining ->
                        False

                    Unicode.MarkEnclosing ->
                        False

                    Unicode.NumberDecimalDigit ->
                        False

                    Unicode.NumberLetter ->
                        False

                    Unicode.NumberOther ->
                        False

                    Unicode.SeparatorSpace ->
                        True

                    Unicode.LetterModifier ->
                        False

                    Unicode.LetterOther ->
                        False

                    Unicode.PunctuationConnector ->
                        False

                    Unicode.PunctuationDash ->
                        False

                    Unicode.PunctuationOpen ->
                        False

                    Unicode.PunctuationClose ->
                        False

                    Unicode.PunctuationInitialQuote ->
                        False

                    Unicode.PunctuationFinalQuote ->
                        False

                    Unicode.PunctuationOther ->
                        False

                    Unicode.SymbolMath ->
                        False

                    Unicode.SymbolCurrency ->
                        False

                    Unicode.SymbolModifier ->
                        False

                    Unicode.SymbolOther ->
                        False


pattern :
    ModuleOriginLookup
    -> Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern
    -> Result String RocPattern
pattern moduleOriginLookup (Elm.Syntax.Node.Node _ syntaxPattern) =
    -- IGNORE TCO
    case syntaxPattern of
        Elm.Syntax.Pattern.AllPattern ->
            Ok RocPatternIgnore

        Elm.Syntax.Pattern.UnitPattern ->
            Ok RocPatternIgnore

        Elm.Syntax.Pattern.CharPattern charValue ->
            Ok (RocPatternInteger (charValue |> Char.toCode))

        Elm.Syntax.Pattern.StringPattern stringValue ->
            Ok (RocPatternString stringValue)

        Elm.Syntax.Pattern.IntPattern intValue ->
            Ok (RocPatternInteger intValue)

        Elm.Syntax.Pattern.HexPattern intValue ->
            Ok (RocPatternInteger intValue)

        Elm.Syntax.Pattern.FloatPattern _ ->
            Err "float pattern is invalid syntax"

        Elm.Syntax.Pattern.VarPattern variableName ->
            Ok (RocPatternVariable variableName)

        Elm.Syntax.Pattern.ParenthesizedPattern inParens ->
            pattern moduleOriginLookup inParens

        Elm.Syntax.Pattern.TuplePattern parts ->
            case parts of
                [] ->
                    -- should be covered by UnitPattern
                    Ok RocPatternIgnore

                [ inParens ] ->
                    -- should be covered by ParenthesizedPattern
                    pattern moduleOriginLookup inParens

                [ part0Node, part1Node ] ->
                    Result.map2
                        (\part0 part1 -> RocPatternTuple { part0 = part0, part1 = part1 })
                        (part0Node |> pattern moduleOriginLookup)
                        (part1Node |> pattern moduleOriginLookup)

                [ part0Node, part1Node, part2Node ] ->
                    Result.map3
                        (\part0 part1 part2 ->
                            RocPatternTriple { part0 = part0, part1 = part1, part2 = part2 }
                        )
                        (part0Node |> pattern moduleOriginLookup)
                        (part1Node |> pattern moduleOriginLookup)
                        (part2Node |> pattern moduleOriginLookup)

                _ :: _ :: _ :: _ :: _ ->
                    -- invalid syntax
                    Err "too many tuple parts"

        Elm.Syntax.Pattern.RecordPattern fields ->
            Ok (RocPatternRecord (fields |> List.map Elm.Syntax.Node.value |> FastSet.fromList))

        Elm.Syntax.Pattern.UnConsPattern headPattern tailPattern ->
            let
                tailExpanded :
                    { initialElements : List (Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern)
                    , tail : Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern
                    }
                tailExpanded =
                    tailPattern |> patternConsExpand
            in
            resultAndThen3
                (\initialElement0 initialElement1Up tailRocPattern ->
                    case tailRocPattern of
                        RocPatternVariable variable ->
                            Ok
                                (RocPatternListCons
                                    { initialElement0 = initialElement0
                                    , initialElement1Up = initialElement1Up
                                    , tailVariable = Just variable
                                    }
                                )

                        RocPatternIgnore ->
                            Ok
                                (RocPatternListCons
                                    { initialElement0 = initialElement0
                                    , initialElement1Up = initialElement1Up
                                    , tailVariable = Nothing
                                    }
                                )

                        RocPatternListExact tailExactElements ->
                            Ok
                                (RocPatternListExact
                                    (initialElement0
                                        :: initialElement1Up
                                        ++ tailExactElements
                                    )
                                )

                        RocPatternInteger _ ->
                            Err "list tail pattern can only be ignored or a variable"

                        RocPatternString _ ->
                            Err "list tail pattern can only be ignored or a variable"

                        RocPatternRecord _ ->
                            Err "list tail pattern can only be ignored or a variable"

                        RocPatternAs _ ->
                            Err "list tail pattern can only be ignored or a variable"

                        RocPatternListCons _ ->
                            Err "list tail pattern can only be ignored or a variable"

                        RocPatternTagged _ ->
                            Err "list tail pattern can only be ignored or a variable"

                        RocPatternTuple _ ->
                            Err "list tail pattern can only be ignored or a variable"

                        RocPatternTriple _ ->
                            Err "list tail pattern can only be ignored or a variable"
                )
                (headPattern |> pattern moduleOriginLookup)
                (tailExpanded.initialElements
                    |> listMapAndCombineOk
                        (\initialElement -> initialElement |> pattern moduleOriginLookup)
                )
                (tailExpanded.tail |> pattern moduleOriginLookup)

        Elm.Syntax.Pattern.ListPattern elementPatterns ->
            Result.map (\elements -> RocPatternListExact elements)
                (elementPatterns
                    |> listMapAndCombineOk
                        (\element -> element |> pattern moduleOriginLookup)
                )

        Elm.Syntax.Pattern.NamedPattern syntaxQualifiedNameRef argumentPatterns ->
            case moduleOriginLookup |> FastDict.get ( syntaxQualifiedNameRef.moduleName, syntaxQualifiedNameRef.name ) of
                Nothing ->
                    Err
                        ("origin choice type not found for the variant "
                            ++ (syntaxQualifiedNameRef.moduleName
                                    |> List.map (\part -> part ++ ".")
                                    |> String.concat
                               )
                            ++ syntaxQualifiedNameRef.name
                        )

                Just moduleOrigin ->
                    Result.map
                        (\parameters ->
                            RocPatternTagged
                                { parameters = parameters
                                , name =
                                    case { moduleOrigin = moduleOrigin, name = syntaxQualifiedNameRef.name } |> referenceToRoc of
                                        Just rocReference ->
                                            rocReference.name

                                        Nothing ->
                                            referenceToRocName
                                                { moduleOrigin = moduleOrigin
                                                , name = syntaxQualifiedNameRef.name
                                                }
                                }
                        )
                        (argumentPatterns
                            |> listMapAndCombineOk
                                (\argument -> argument |> pattern moduleOriginLookup)
                        )

        Elm.Syntax.Pattern.AsPattern aliasedPatternNode (Elm.Syntax.Node.Node _ variable) ->
            Result.map
                (\aliasedPattern ->
                    RocPatternAs
                        { pattern = aliasedPattern, variable = variable }
                )
                (aliasedPatternNode |> pattern moduleOriginLookup)


referenceToRoc :
    { moduleOrigin : Elm.Syntax.ModuleName.ModuleName
    , name : String
    }
    ->
        Maybe
            { moduleOrigin : Maybe String
            , name : String
            }
referenceToRoc reference =
    case reference.moduleOrigin of
        [ "Basics" ] ->
            case reference.name of
                "LT" ->
                    Just { moduleOrigin = Nothing, name = "LT" }

                "EQ" ->
                    Just { moduleOrigin = Nothing, name = "EQ" }

                "GT" ->
                    Just { moduleOrigin = Nothing, name = "GT" }

                "True" ->
                    Just { moduleOrigin = Just "Bool", name = "true" }

                "False" ->
                    Just { moduleOrigin = Just "Bool", name = "false" }

                "not" ->
                    Just { moduleOrigin = Just "Bool", name = "not" }

                "xor" ->
                    Just { moduleOrigin = Just "Bool", name = "isNotEq" }

                "e" ->
                    Just { moduleOrigin = Just "Num", name = "e" }

                "pi" ->
                    Just { moduleOrigin = Just "Num", name = "pi" }

                "ceiling" ->
                    Just { moduleOrigin = Just "Num", name = "ceiling" }

                "floor" ->
                    Just { moduleOrigin = Just "Num", name = "floor" }

                "round" ->
                    Just { moduleOrigin = Just "Num", name = "round" }

                "negate" ->
                    Just { moduleOrigin = Just "Num", name = "negate" }

                "abs" ->
                    Just { moduleOrigin = Just "Num", name = "abs" }

                "toFloat" ->
                    Just { moduleOrigin = Just "Num", name = "toFrac" }

                "isNaN" ->
                    Just { moduleOrigin = Just "Num", name = "isNan" }

                "isInfinite" ->
                    Just { moduleOrigin = Just "Num", name = "isInfinite" }

                "remainderBy" ->
                    Just { moduleOrigin = Nothing, name = "basicsRemainderBy" }

                "modBy" ->
                    Just { moduleOrigin = Nothing, name = "basicsModBy" }

                "sin" ->
                    Just { moduleOrigin = Just "Num", name = "sin" }

                "cos" ->
                    Just { moduleOrigin = Just "Num", name = "cos" }

                "tan" ->
                    Just { moduleOrigin = Just "Num", name = "tan" }

                "asin" ->
                    Just { moduleOrigin = Just "Num", name = "asin" }

                "acos" ->
                    Just { moduleOrigin = Just "Num", name = "acos" }

                "atan" ->
                    Just { moduleOrigin = Just "Num", name = "atan" }

                "sqrt" ->
                    Just { moduleOrigin = Just "Num", name = "sqrt" }

                "compare" ->
                    Just { moduleOrigin = Just "Num", name = "compare" }

                "max" ->
                    Just { moduleOrigin = Just "Num", name = "max" }

                "min" ->
                    Just { moduleOrigin = Just "Num", name = "min" }

                "identity" ->
                    Just { moduleOrigin = Nothing, name = "basicsIdentity" }

                "always" ->
                    Just { moduleOrigin = Nothing, name = "basicsAlways" }

                _ ->
                    Nothing

        [ "String" ] ->
            case reference.name of
                "isEmpty" ->
                    Just { moduleOrigin = Just "Str", name = "isEmpty" }

                "length" ->
                    Just { moduleOrigin = Just "Str", name = "countUtf8Bytes" }

                "append" ->
                    Just { moduleOrigin = Just "Str", name = "concat" }

                "trim" ->
                    Just { moduleOrigin = Just "Str", name = "trim" }

                "trimLeft" ->
                    Just { moduleOrigin = Just "Str", name = "trimStart" }

                "trimRight" ->
                    Just { moduleOrigin = Just "Str", name = "trimEnd" }

                "toList" ->
                    Just { moduleOrigin = Just "Str", name = "toUtf8" }

                "join" ->
                    Just { moduleOrigin = Nothing, name = "stringJoin" }

                "split" ->
                    Just { moduleOrigin = Nothing, name = "stringSplit" }

                "repeat" ->
                    Just { moduleOrigin = Nothing, name = "stringRepeat" }

                "startsWith" ->
                    Just { moduleOrigin = Nothing, name = "stringStartsWith" }

                "endsWith" ->
                    Just { moduleOrigin = Nothing, name = "stringEndsWith" }

                "toInt" ->
                    Just { moduleOrigin = Nothing, name = "stringToInt" }

                "toFloat" ->
                    Just { moduleOrigin = Nothing, name = "stringToFloat" }

                "contains" ->
                    Just { moduleOrigin = Nothing, name = "stringContains" }

                "any" ->
                    Just { moduleOrigin = Nothing, name = "stringAny" }

                "all" ->
                    Just { moduleOrigin = Nothing, name = "stringAll" }

                "fromChar" ->
                    Just { moduleOrigin = Nothing, name = "stringFromChar" }

                "cons" ->
                    Just { moduleOrigin = Nothing, name = "stringCons" }

                "slice" ->
                    Just { moduleOrigin = Nothing, name = "stringSlice" }

                "toLower" ->
                    Just { moduleOrigin = Nothing, name = "stringToLower" }

                "toUpper" ->
                    Just { moduleOrigin = Nothing, name = "stringToUpper" }

                _ ->
                    Nothing

        [ "Char" ] ->
            case reference.name of
                "toCode" ->
                    Just { moduleOrigin = Just "Num", name = "intCast" }

                "fromCode" ->
                    Just { moduleOrigin = Just "Num", name = "intCast" }

                "toLower" ->
                    Just { moduleOrigin = Nothing, name = "charToLower" }

                "toUpper" ->
                    Just { moduleOrigin = Nothing, name = "charToUpper" }

                _ ->
                    Nothing

        [ "List" ] ->
            case reference.name of
                "singleton" ->
                    Just { moduleOrigin = Just "List", name = "single" }

                "isEmpty" ->
                    Just { moduleOrigin = Just "List", name = "isEmpty" }

                "length" ->
                    Just { moduleOrigin = Just "List", name = "len" }

                "sum" ->
                    Just { moduleOrigin = Just "List", name = "sum" }

                "product" ->
                    Just { moduleOrigin = Just "List", name = "product" }

                "append" ->
                    Just { moduleOrigin = Just "List", name = "concat" }

                "concat" ->
                    Just { moduleOrigin = Just "List", name = "join" }

                "sort" ->
                    Just { moduleOrigin = Just "List", name = "sortAsc" }

                "reverse" ->
                    Just { moduleOrigin = Just "List", name = "reverse" }

                "repeat" ->
                    Just { moduleOrigin = Nothing, name = "listRepeat" }

                "all" ->
                    Just { moduleOrigin = Nothing, name = "listAll" }

                "any" ->
                    Just { moduleOrigin = Nothing, name = "listAny" }

                "filter" ->
                    Just { moduleOrigin = Nothing, name = "listFilter" }

                "map" ->
                    Just { moduleOrigin = Nothing, name = "listMap" }

                "map2" ->
                    Just { moduleOrigin = Nothing, name = "listMap2" }

                "map3" ->
                    Just { moduleOrigin = Nothing, name = "listMap3" }

                "map4" ->
                    Just { moduleOrigin = Nothing, name = "listMap4" }

                "concatMap" ->
                    Just { moduleOrigin = Nothing, name = "listConcatMap" }

                "sortWith" ->
                    Just { moduleOrigin = Nothing, name = "listSortWith" }

                "range" ->
                    Just { moduleOrigin = Nothing, name = "listRange" }

                "take" ->
                    Just { moduleOrigin = Nothing, name = "listTake" }

                "drop" ->
                    Just { moduleOrigin = Nothing, name = "listDrop" }

                "intersperse" ->
                    Just { moduleOrigin = Nothing, name = "listIntersperse" }

                "foldl" ->
                    Just { moduleOrigin = Nothing, name = "listFoldl" }

                "foldr" ->
                    Just { moduleOrigin = Nothing, name = "listFoldr" }

                _ ->
                    Nothing

        [ "Dict" ] ->
            case reference.name of
                "empty" ->
                    Just { moduleOrigin = Just "Dict", name = "empty" }

                "singleton" ->
                    Just { moduleOrigin = Just "Dict", name = "single" }

                "fromList" ->
                    Just { moduleOrigin = Just "Dict", name = "fromList" }

                "toList" ->
                    Just { moduleOrigin = Just "Dict", name = "toList" }

                "keys" ->
                    Just { moduleOrigin = Just "Dict", name = "keys" }

                "values" ->
                    Just { moduleOrigin = Just "Dict", name = "values" }

                "size" ->
                    Just { moduleOrigin = Just "Dict", name = "len" }

                "isEmpty" ->
                    Just { moduleOrigin = Just "Dict", name = "isEmpty" }

                "diff" ->
                    Just { moduleOrigin = Just "Dict", name = "removeAll" }

                "map" ->
                    Just { moduleOrigin = Nothing, name = "dictMap" }

                "foldl" ->
                    Just { moduleOrigin = Nothing, name = "dictFoldl" }

                "foldr" ->
                    Just { moduleOrigin = Nothing, name = "dictFoldr" }

                "get" ->
                    Just { moduleOrigin = Nothing, name = "dictGet" }

                "member" ->
                    Just { moduleOrigin = Nothing, name = "dictMember" }

                "insert" ->
                    Just { moduleOrigin = Nothing, name = "dictInsert" }

                "remove" ->
                    Just { moduleOrigin = Nothing, name = "dictRemove" }

                "union" ->
                    Just { moduleOrigin = Nothing, name = "dictUnion" }

                "filter" ->
                    Just { moduleOrigin = Nothing, name = "dictFilter" }

                _ ->
                    Nothing

        [ "Parser" ] ->
            case reference.name of
                "Expecting" ->
                    Just { moduleOrigin = Nothing, name = "ParserExpecting" }

                "ExpectingInt" ->
                    Just { moduleOrigin = Nothing, name = "ParserExpectingInt" }

                "ExpectingHex" ->
                    Just { moduleOrigin = Nothing, name = "ParserExpectingHex" }

                "ExpectingOctal" ->
                    Just { moduleOrigin = Nothing, name = "ParserExpectingOctal" }

                "ExpectingBinary" ->
                    Just { moduleOrigin = Nothing, name = "ParserExpectingBinary" }

                "ExpectingFloat" ->
                    Just { moduleOrigin = Nothing, name = "ParserExpectingFloat" }

                "ExpectingNumber" ->
                    Just { moduleOrigin = Nothing, name = "ParserExpectingNumber" }

                "ExpectingVariable" ->
                    Just { moduleOrigin = Nothing, name = "ParserExpectingVariable" }

                "ExpectingSymbol" ->
                    Just { moduleOrigin = Nothing, name = "ParserExpectingSymbol" }

                "ExpectingKeyword" ->
                    Just { moduleOrigin = Nothing, name = "ParserExpectingKeyword" }

                "ExpectingEnd" ->
                    Just { moduleOrigin = Nothing, name = "ParserExpectingEnd" }

                "UnexpectedChar" ->
                    Just { moduleOrigin = Nothing, name = "ParserUnexpectedChar" }

                "Problem" ->
                    Just { moduleOrigin = Nothing, name = "ParserProblem" }

                "BadRepeat" ->
                    Just { moduleOrigin = Nothing, name = "ParserBadRepeat" }

                _ ->
                    Nothing

        _ ->
            Nothing


referenceToRocName :
    { moduleOrigin : Elm.Syntax.ModuleName.ModuleName
    , name : String
    }
    -> String
referenceToRocName reference =
    ((if reference.name |> stringFirstCharIsUpper then
        reference.moduleOrigin |> String.concat

      else
        reference.moduleOrigin |> String.concat |> stringFirstCharToLower
     )
        ++ (reference.name |> stringFirstCharToUpper)
    )
        |> rocNameSanitize


rocNameSanitize : String -> String
rocNameSanitize unsanitizedName =
    case unsanitizedName of
        "is" ->
            "is0"

        "crash" ->
            "crash0"

        "dbg" ->
            "dbg0"

        "expect" ->
            "expect0"

        "return" ->
            "return0"

        "try" ->
            "try0"

        "when" ->
            "when0"

        unsanitizedNameNotKeyword ->
            unsanitizedNameNotKeyword
                |> String.map
                    (\char ->
                        if char |> Char.isAlphaNum then
                            char

                        else
                            '0'
                    )


printRocPatternNotParenthesized : RocPattern -> Print
printRocPatternNotParenthesized rocPattern =
    -- IGNORE TCO
    case rocPattern of
        RocPatternIgnore ->
            printExactlyUnderscore

        RocPatternInteger intValue ->
            Print.exactly (String.fromInt intValue)

        RocPatternString string ->
            stringLiteral string

        RocPatternVariable name ->
            Print.exactly name

        RocPatternRecord fields ->
            printRocPatternRecord fields

        RocPatternListCons rocPatternListCons ->
            printRocPatternCons rocPatternListCons

        RocPatternListExact elements ->
            printPatternListExact elements

        RocPatternTagged tagged ->
            Print.exactly tagged.name
                |> Print.followedBy
                    (case tagged.parameters of
                        [] ->
                            Print.empty

                        parameter0 :: parameter1Up ->
                            Print.exactly " "
                                |> Print.followedBy
                                    ((parameter0 :: parameter1Up)
                                        |> Print.listMapAndIntersperseAndFlatten
                                            printRocPatternNotParenthesized
                                            (Print.exactly " ")
                                    )
                    )

        RocPatternAs patternAs ->
            printRocPatternAs patternAs

        RocPatternTuple parts ->
            Print.exactly "( "
                |> Print.followedBy
                    ([ parts.part0, parts.part1 ]
                        |> Print.listMapAndIntersperseAndFlatten
                            printRocPatternNotParenthesized
                            (Print.exactly ", ")
                    )
                |> Print.followedBy (Print.exactly " )")

        RocPatternTriple parts ->
            Print.exactly "( "
                |> Print.followedBy
                    ([ parts.part0, parts.part1, parts.part2 ]
                        |> Print.listMapAndIntersperseAndFlatten
                            printRocPatternNotParenthesized
                            (Print.exactly ", ")
                    )
                |> Print.followedBy (Print.exactly " )")


printPatternListExact : List RocPattern -> Print
printPatternListExact elements =
    case elements of
        [] ->
            Print.exactly "[]"

        element0 :: element1Up ->
            let
                elementsPrint : Print
                elementsPrint =
                    (element0 :: element1Up)
                        |> Print.listMapAndIntersperseAndFlatten
                            (\elementNode ->
                                Print.withIndentIncreasedBy 2
                                    (printRocPatternNotParenthesized elementNode)
                            )
                            (Print.linebreakIndented
                                |> Print.followedBy
                                    (Print.exactly ", ")
                            )
            in
            printExactlySquareOpeningSpace
                |> Print.followedBy elementsPrint
                |> Print.followedBy
                    (Print.spaceOrLinebreakIndented
                        (elementsPrint |> Print.lineSpread)
                    )
                |> Print.followedBy printExactlySquareClosing


printRocPatternCons :
    { initialElement0 : RocPattern
    , initialElement1Up : List RocPattern
    , tailVariable : Maybe String
    }
    -> Print
printRocPatternCons syntaxCons =
    printExactlySquareOpeningSpace
        |> Print.FollowedBy
            (syntaxCons.initialElement0
                :: syntaxCons.initialElement1Up
                |> Print.listMapAndIntersperseAndFlatten
                    (\initialElement ->
                        printRocPatternNotParenthesized initialElement
                    )
                    (Print.exactly ", ")
            )
        |> Print.followedBy
            (Print.exactly ", ..")
        |> Print.followedBy
            (case syntaxCons.tailVariable of
                Nothing ->
                    Print.empty

                Just tailVariable ->
                    Print.exactly (" as " ++ tailVariable)
            )
        |> Print.followedBy
            (Print.exactly " ]")


patternConsExpand :
    Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern
    ->
        { initialElements : List (Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern)
        , tail : Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern
        }
patternConsExpand patternNode =
    patternConsExpandFromInitialElementsReverse [] patternNode


patternConsExpandFromInitialElementsReverse :
    List (Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern)
    -> Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern
    ->
        { initialElements : List (Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern)
        , tail : Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern
        }
patternConsExpandFromInitialElementsReverse initialElementsSoFarReverse (Elm.Syntax.Node.Node fulRange syntaxPattern) =
    case syntaxPattern of
        Elm.Syntax.Pattern.UnConsPattern headPattern tailPattern ->
            patternConsExpandFromInitialElementsReverse
                (headPattern :: initialElementsSoFarReverse)
                tailPattern

        Elm.Syntax.Pattern.ParenthesizedPattern inParens ->
            patternConsExpandFromInitialElementsReverse initialElementsSoFarReverse
                inParens

        Elm.Syntax.Pattern.AllPattern ->
            { initialElements = initialElementsSoFarReverse |> List.reverse
            , tail = Elm.Syntax.Node.Node fulRange Elm.Syntax.Pattern.AllPattern
            }

        Elm.Syntax.Pattern.UnitPattern ->
            { initialElements = initialElementsSoFarReverse |> List.reverse
            , tail = Elm.Syntax.Node.Node fulRange Elm.Syntax.Pattern.UnitPattern
            }

        Elm.Syntax.Pattern.CharPattern char ->
            { initialElements = initialElementsSoFarReverse |> List.reverse
            , tail = Elm.Syntax.Node.Node fulRange (Elm.Syntax.Pattern.CharPattern char)
            }

        Elm.Syntax.Pattern.StringPattern string ->
            { initialElements = initialElementsSoFarReverse |> List.reverse
            , tail = Elm.Syntax.Node.Node fulRange (Elm.Syntax.Pattern.StringPattern string)
            }

        Elm.Syntax.Pattern.IntPattern int ->
            { initialElements = initialElementsSoFarReverse |> List.reverse
            , tail = Elm.Syntax.Node.Node fulRange (Elm.Syntax.Pattern.IntPattern int)
            }

        Elm.Syntax.Pattern.HexPattern int ->
            { initialElements = initialElementsSoFarReverse |> List.reverse
            , tail = Elm.Syntax.Node.Node fulRange (Elm.Syntax.Pattern.HexPattern int)
            }

        Elm.Syntax.Pattern.FloatPattern float ->
            { initialElements = initialElementsSoFarReverse |> List.reverse
            , tail = Elm.Syntax.Node.Node fulRange (Elm.Syntax.Pattern.FloatPattern float)
            }

        Elm.Syntax.Pattern.TuplePattern parts ->
            case parts of
                [ part0, part1 ] ->
                    { initialElements = initialElementsSoFarReverse |> List.reverse
                    , tail = Elm.Syntax.Node.Node fulRange (Elm.Syntax.Pattern.TuplePattern [ part0, part1 ])
                    }

                [ part0, part1, part2 ] ->
                    { initialElements = initialElementsSoFarReverse |> List.reverse
                    , tail = Elm.Syntax.Node.Node fulRange (Elm.Syntax.Pattern.TuplePattern [ part0, part1, part2 ])
                    }

                [] ->
                    -- should be handled by UnitPattern
                    { initialElements = initialElementsSoFarReverse |> List.reverse
                    , tail = Elm.Syntax.Node.Node fulRange Elm.Syntax.Pattern.UnitPattern
                    }

                [ inParens ] ->
                    -- should be handled by ParenthesizedPattern
                    { initialElements = initialElementsSoFarReverse |> List.reverse
                    , tail = Elm.Syntax.Node.Node fulRange (Elm.Syntax.Pattern.TuplePattern [ inParens ])
                    }

                part0 :: part1 :: part2 :: part3 :: part4Up ->
                    -- should be handled by ParenthesizedPattern
                    { initialElements = initialElementsSoFarReverse |> List.reverse
                    , tail =
                        Elm.Syntax.Node.Node fulRange
                            (Elm.Syntax.Pattern.TuplePattern (part0 :: part1 :: part2 :: part3 :: part4Up))
                    }

        Elm.Syntax.Pattern.RecordPattern fields ->
            { initialElements = initialElementsSoFarReverse |> List.reverse
            , tail = Elm.Syntax.Node.Node fulRange (Elm.Syntax.Pattern.RecordPattern fields)
            }

        Elm.Syntax.Pattern.ListPattern elements ->
            { initialElements = initialElementsSoFarReverse |> List.reverse
            , tail = Elm.Syntax.Node.Node fulRange (Elm.Syntax.Pattern.ListPattern elements)
            }

        Elm.Syntax.Pattern.VarPattern variableName ->
            { initialElements = initialElementsSoFarReverse |> List.reverse
            , tail = Elm.Syntax.Node.Node fulRange (Elm.Syntax.Pattern.VarPattern variableName)
            }

        Elm.Syntax.Pattern.NamedPattern reference parameters ->
            { initialElements = initialElementsSoFarReverse |> List.reverse
            , tail = Elm.Syntax.Node.Node fulRange (Elm.Syntax.Pattern.NamedPattern reference parameters)
            }

        Elm.Syntax.Pattern.AsPattern aliasedPattern aliasName ->
            { initialElements = initialElementsSoFarReverse |> List.reverse
            , tail = Elm.Syntax.Node.Node fulRange (Elm.Syntax.Pattern.AsPattern aliasedPattern aliasName)
            }


printRocPatternAs :
    { variable : String
    , pattern : RocPattern
    }
    -> Print
printRocPatternAs syntaxAs =
    printParenthesized (printRocPatternNotParenthesized syntaxAs.pattern)
        |> Print.followedBy (Print.exactly (" as " ++ syntaxAs.variable))


printRocPatternRecord : FastSet.Set String -> Print
printRocPatternRecord syntaxRecordFields =
    printExactlyCurlyOpeningSpace
        |> Print.followedBy
            (syntaxRecordFields
                |> FastSet.toList
                |> Print.listMapAndIntersperseAndFlatten
                    Print.exactly
                    (Print.exactly ", ")
            )
        |> Print.followedBy
            (Print.exactly " ")
        |> Print.followedBy printExactlyCurlyClosing


printRocExpressionRecord : FastDict.Dict String RocExpression -> Print
printRocExpressionRecord syntaxRecordFields =
    if syntaxRecordFields |> FastDict.isEmpty then
        Print.exactly "{}"

    else
        let
            fieldsPrint : Print
            fieldsPrint =
                syntaxRecordFields
                    |> FastDict.toList
                    |> Print.listReverseAndMapAndIntersperseAndFlatten
                        (\( fieldName, fieldValue ) ->
                            Print.withIndentIncreasedBy 2
                                (Print.exactly (fieldName ++ ":")
                                    |> Print.followedBy
                                        (Print.withIndentAtNextMultipleOf4
                                            (Print.linebreakIndented
                                                |> Print.followedBy
                                                    (printRocExpressionNotParenthesized fieldValue)
                                            )
                                        )
                                )
                        )
                        (Print.linebreakIndented
                            |> Print.followedBy
                                (Print.exactly ", ")
                        )
        in
        printExactlyCurlyOpeningSpace
            |> Print.followedBy fieldsPrint
            |> Print.followedBy
                (Print.spaceOrLinebreakIndented
                    (fieldsPrint |> Print.lineSpread)
                )
            |> Print.followedBy printExactlyCurlyClosing


printParenthesized : Print -> Print
printParenthesized notParenthesizedPrint =
    let
        lineSpread : Print.LineSpread
        lineSpread =
            notParenthesizedPrint
                |> Print.lineSpread
    in
    printExactlyParensOpening
        |> Print.followedBy
            (Print.withIndentIncreasedBy 1
                notParenthesizedPrint
            )
        |> Print.followedBy
            (Print.emptyOrLinebreakIndented lineSpread)
        |> Print.followedBy printExactlyParensClosing


{-| Transpile a list of [`Elm.Syntax.Declaration.Declaration`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Syntax-Declaration#Declaration)s
across multiple modules to [`RocDeclaration`]s.
Declarations that use unsupported stuff like parser kernel code (directly or indirectly)
will not be present in the final declarations.
Their errors can be found alongside the valid transpiled declarations.

The given list of files must also include files from used dependencies
including `elm/core`.
-}
modules :
    List Elm.Syntax.File.File
    ->
        { errors : List String
        , declarations :
            List
                { name : String
                , expression : RocExpression
                }
        }
modules syntaxDeclarationsIncludingCore =
    let
        syntaxModules : List Elm.Syntax.File.File
        syntaxModules =
            syntaxDeclarationsIncludingCore
                |> List.filter
                    (\syntaxModule ->
                        case
                            syntaxModule.moduleDefinition
                                |> Elm.Syntax.Node.value
                                |> moduleHeaderName
                        of
                            [ "Basics" ] ->
                                False

                            [ "Array" ] ->
                                False

                            [ "Bitwise" ] ->
                                False

                            [ "Debug" ] ->
                                False

                            [ "Char" ] ->
                                False

                            [ "List" ] ->
                                False

                            [ "Dict" ] ->
                                False

                            [ "Set" ] ->
                                False

                            [ "Platform" ] ->
                                False

                            [ "Platform", "Cmd" ] ->
                                False

                            [ "Platform", "Sub" ] ->
                                False

                            [ "Process" ] ->
                                False

                            [ "Task" ] ->
                                False

                            [ "Json", "Decode" ] ->
                                False

                            [ "Json", "Encode" ] ->
                                False

                            [ "Parser" ] ->
                                False

                            [ "Parser", "Advanced" ] ->
                                False

                            [ "Regex" ] ->
                                False

                            [ "File" ] ->
                                False

                            [ "Bytes" ] ->
                                False

                            [ "Bytes", "Encode" ] ->
                                False

                            [ "Bytes", "Decode" ] ->
                                False

                            [ "Http" ] ->
                                False

                            [ "VirtualDom" ] ->
                                False

                            [ "Browser" ] ->
                                False

                            [ "Browser", "Events" ] ->
                                False

                            [ "Browser", "Navigation" ] ->
                                False

                            [ "Browser", "Dom" ] ->
                                False

                            [ "Html" ] ->
                                False

                            [ "Html", "Attributes" ] ->
                                False

                            [ "Html", "Events" ] ->
                                False

                            [ "Html", "Keyed" ] ->
                                False

                            [ "Html", "Lazy" ] ->
                                False

                            [ "Svg" ] ->
                                False

                            [ "Svg", "Attributes" ] ->
                                False

                            [ "Svg", "Events" ] ->
                                False

                            [ "Svg", "Keyed" ] ->
                                False

                            [ "Svg", "Lazy" ] ->
                                False

                            [ "Time" ] ->
                                False

                            [ "Random" ] ->
                                False

                            [ "Url" ] ->
                                False

                            [ "Url", "Builder" ] ->
                                False

                            [ "Url", "Parser" ] ->
                                False

                            [ "Url", "Parser", "Query" ] ->
                                False

                            [ "Markdown" ] ->
                                False

                            [ "Benchmark" ] ->
                                False

                            [ "WebGL" ] ->
                                False

                            [ "WebGL", "Settings" ] ->
                                False

                            [ "WebGL", "Settings", "Blend" ] ->
                                False

                            [ "WebGL", "Settings", "DepthTest" ] ->
                                False

                            [ "WebGL", "Settings", "StencilTest" ] ->
                                False

                            [ "WebGL", "Texture" ] ->
                                False

                            [ "Math", "Matrix4" ] ->
                                False

                            [ "Math", "Vector2" ] ->
                                False

                            [ "Math", "Vector3" ] ->
                                False

                            [ "Math", "Vector4" ] ->
                                False

                            _ ->
                                True
                    )

        moduleMembers :
            FastDict.Dict
                Elm.Syntax.ModuleName.ModuleName
                { valueOrFunctionNames : FastSet.Set String
                , choiceTypesExposingVariants : FastDict.Dict String (FastSet.Set String)
                }
        moduleMembers =
            syntaxModules
                |> List.foldl
                    (\syntaxModule soFar ->
                        soFar
                            |> FastDict.insert
                                (syntaxModule.moduleDefinition
                                    |> Elm.Syntax.Node.value
                                    |> moduleHeaderName
                                )
                                (syntaxModule.declarations
                                    |> List.foldl
                                        (\(Elm.Syntax.Node.Node _ declaration) membersSoFar ->
                                            case declaration of
                                                Elm.Syntax.Declaration.FunctionDeclaration syntaxValueOrFunctionDeclaration ->
                                                    { valueOrFunctionNames =
                                                        membersSoFar.valueOrFunctionNames
                                                            |> FastSet.insert
                                                                (syntaxValueOrFunctionDeclaration.declaration
                                                                    |> Elm.Syntax.Node.value
                                                                    |> .name
                                                                    |> Elm.Syntax.Node.value
                                                                    |> rocNameSanitize
                                                                )
                                                    , choiceTypesExposingVariants =
                                                        membersSoFar.choiceTypesExposingVariants
                                                    }

                                                Elm.Syntax.Declaration.CustomTypeDeclaration choiceTypeDeclaration ->
                                                    { valueOrFunctionNames =
                                                        membersSoFar.valueOrFunctionNames
                                                    , choiceTypesExposingVariants =
                                                        membersSoFar.choiceTypesExposingVariants
                                                            |> FastDict.insert
                                                                (choiceTypeDeclaration.name |> Elm.Syntax.Node.value)
                                                                (choiceTypeDeclaration.constructors
                                                                    |> List.foldl
                                                                        (\(Elm.Syntax.Node.Node _ variant) variantNamesSoFar ->
                                                                            variantNamesSoFar
                                                                                |> FastSet.insert
                                                                                    (variant.name
                                                                                        |> Elm.Syntax.Node.value
                                                                                        |> rocNameSanitize
                                                                                    )
                                                                        )
                                                                        FastSet.empty
                                                                )
                                                    }

                                                Elm.Syntax.Declaration.AliasDeclaration _ ->
                                                    membersSoFar

                                                Elm.Syntax.Declaration.PortDeclaration _ ->
                                                    -- not supported
                                                    membersSoFar

                                                Elm.Syntax.Declaration.InfixDeclaration _ ->
                                                    membersSoFar

                                                Elm.Syntax.Declaration.Destructuring _ _ ->
                                                    -- invalid syntax
                                                    membersSoFar
                                        )
                                        { valueOrFunctionNames = FastSet.empty
                                        , choiceTypesExposingVariants = FastDict.empty
                                        }
                                )
                    )
                    FastDict.empty
    in
    syntaxModules
        |> List.foldr
            (\syntaxModule soFarAcrossModules ->
                let
                    moduleName : Elm.Syntax.ModuleName.ModuleName
                    moduleName =
                        syntaxModule.moduleDefinition
                            |> Elm.Syntax.Node.value
                            |> moduleHeaderName

                    moduleOriginLookup : ModuleOriginLookup
                    moduleOriginLookup =
                        FastDict.union
                            (syntaxModule.imports |> importsToModuleOriginLookup moduleMembers)
                            (case moduleMembers |> FastDict.get moduleName of
                                Nothing ->
                                    FastDict.empty

                                Just moduleLocalNames ->
                                    FastSet.union
                                        moduleLocalNames.valueOrFunctionNames
                                        (moduleLocalNames.choiceTypesExposingVariants
                                            |> FastDict.foldl
                                                (\_ variantNames soFar ->
                                                    FastSet.union variantNames soFar
                                                )
                                                FastSet.empty
                                        )
                                        |> FastSet.foldl
                                            (\name soFar ->
                                                soFar
                                                    |> FastDict.insert ( [], name )
                                                        moduleName
                                            )
                                            FastDict.empty
                            )
                in
                syntaxModule.declarations
                    |> List.foldr
                        (\(Elm.Syntax.Node.Node _ declaration) soFar ->
                            case declaration of
                                Elm.Syntax.Declaration.FunctionDeclaration syntaxValueOrFunctionDeclaration ->
                                    case
                                        syntaxValueOrFunctionDeclaration.declaration
                                            |> Elm.Syntax.Node.value
                                            |> valueOrFunctionDeclaration moduleOriginLookup
                                    of
                                        Ok rocDeclaration ->
                                            { errors = soFar.errors
                                            , declarations =
                                                { rocDeclaration
                                                    | name =
                                                        { moduleOrigin = moduleName
                                                        , name = rocDeclaration.name
                                                        }
                                                            |> referenceToRocName
                                                            |> rocNameSanitize
                                                }
                                                    :: soFar.declarations
                                            }

                                        Err error ->
                                            { declarations = soFar.declarations
                                            , errors = error :: soFar.errors
                                            }

                                Elm.Syntax.Declaration.AliasDeclaration _ ->
                                    soFar

                                Elm.Syntax.Declaration.CustomTypeDeclaration _ ->
                                    soFar

                                Elm.Syntax.Declaration.PortDeclaration _ ->
                                    soFar

                                Elm.Syntax.Declaration.InfixDeclaration _ ->
                                    soFar

                                Elm.Syntax.Declaration.Destructuring _ _ ->
                                    soFar
                        )
                        soFarAcrossModules
            )
            { errors = []
            , declarations = []
            }


moduleHeaderName : Elm.Syntax.Module.Module -> Elm.Syntax.ModuleName.ModuleName
moduleHeaderName moduleHeader =
    case moduleHeader of
        Elm.Syntax.Module.NormalModule header ->
            header.moduleName |> Elm.Syntax.Node.value

        Elm.Syntax.Module.PortModule header ->
            header.moduleName |> Elm.Syntax.Node.value

        Elm.Syntax.Module.EffectModule header ->
            header.moduleName |> Elm.Syntax.Node.value


valueOrFunctionDeclaration :
    ModuleOriginLookup
    -> Elm.Syntax.Expression.FunctionImplementation
    ->
        Result
            String
            { name : String
            , expression : RocExpression
            }
valueOrFunctionDeclaration moduleOriginLookup syntaxDeclarationValueOrFunction =
    Result.map2
        (\parameters result ->
            { name = syntaxDeclarationValueOrFunction.name |> Elm.Syntax.Node.value
            , expression =
                case parameters of
                    [] ->
                        result

                    parameter0 :: parameter1Up ->
                        RocExpressionLambda
                            { parameter0 = parameter0
                            , parameter1Up = parameter1Up
                            , result = result
                            }
            }
        )
        (syntaxDeclarationValueOrFunction.arguments
            |> listMapAndCombineOk (\p -> p |> pattern moduleOriginLookup)
        )
        (syntaxDeclarationValueOrFunction.expression
            |> expression moduleOriginLookup
        )


expression :
    ModuleOriginLookup
    -> Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
    -> Result String RocExpression
expression moduleOriginLookup (Elm.Syntax.Node.Node _ syntaxExpression) =
    -- IGNORE TCO
    case syntaxExpression of
        Elm.Syntax.Expression.UnitExpr ->
            Ok RocExpressionUnit

        Elm.Syntax.Expression.Integer intValue ->
            Ok (RocExpressionInteger intValue)

        Elm.Syntax.Expression.Hex intValue ->
            Ok (RocExpressionInteger intValue)

        Elm.Syntax.Expression.Floatable floatValue ->
            Ok (RocExpressionF64 floatValue)

        Elm.Syntax.Expression.CharLiteral charValue ->
            Ok (RocExpressionInteger (charValue |> Char.toCode))

        Elm.Syntax.Expression.Literal stringValue ->
            Ok (RocExpressionString stringValue)

        Elm.Syntax.Expression.RecordAccessFunction fieldName ->
            Ok
                (RocExpressionRecordAccessFunction
                    (fieldName |> String.replace "." "")
                )

        Elm.Syntax.Expression.Operator _ ->
            -- invalid syntax
            Err "operator is invalid syntax"

        Elm.Syntax.Expression.PrefixOperator operatorSymbol ->
            Result.map
                (\operationFunctionReference ->
                    RocExpressionReference operationFunctionReference
                )
                (expressionOperatorToRocFunctionReference operatorSymbol)

        Elm.Syntax.Expression.GLSLExpression _ ->
            Err "glsl not supported"

        Elm.Syntax.Expression.Application application ->
            case application of
                [] ->
                    Err "application without any parts is invalid"

                [ inParens ] ->
                    -- invalid syntax
                    expression moduleOriginLookup inParens

                calledNode :: argument0Node :: argument1UpNodes ->
                    Result.map3
                        (\called argument0 argument1Up ->
                            RocExpressionCall
                                { called = called
                                , argument0 = argument0
                                , argument1Up = argument1Up
                                }
                        )
                        (calledNode |> expression moduleOriginLookup)
                        (argument0Node |> expression moduleOriginLookup)
                        (argument1UpNodes
                            |> listMapAndCombineOk
                                (\argument -> argument |> expression moduleOriginLookup)
                        )

        Elm.Syntax.Expression.OperatorApplication operatorSymbol _ leftNode rightNode ->
            case operatorSymbol of
                "|>" ->
                    Result.map2
                        (\argument called ->
                            RocExpressionCall
                                { called = called
                                , argument0 = argument
                                , argument1Up = []
                                }
                        )
                        (leftNode |> expression moduleOriginLookup)
                        (rightNode |> expression moduleOriginLookup)

                "<|" ->
                    Result.map2
                        (\called argument ->
                            RocExpressionCall
                                { called = called
                                , argument0 = argument
                                , argument1Up = []
                                }
                        )
                        (leftNode |> expression moduleOriginLookup)
                        (rightNode |> expression moduleOriginLookup)

                "++" ->
                    Result.map2
                        (\left right ->
                            if
                                (left |> rocExpressionIsDefinitelyOfTypeString)
                                    || (right |> rocExpressionIsDefinitelyOfTypeString)
                            then
                                RocExpressionCall
                                    { called =
                                        RocExpressionReference
                                            { moduleOrigin = Just "Str"
                                            , name = "concat"
                                            }
                                    , argument0 = left
                                    , argument1Up = [ right ]
                                    }

                            else
                                RocExpressionCall
                                    { called =
                                        RocExpressionReference
                                            { moduleOrigin = Just "List"
                                            , name = "concat"
                                            }
                                    , argument0 = left
                                    , argument1Up = [ right ]
                                    }
                        )
                        (leftNode |> expression moduleOriginLookup)
                        (rightNode |> expression moduleOriginLookup)

                otherOperatorSymbol ->
                    Result.map3
                        (\operationFunctionReference left right ->
                            RocExpressionCall
                                { called =
                                    RocExpressionReference operationFunctionReference
                                , argument0 = left
                                , argument1Up = [ right ]
                                }
                        )
                        (expressionOperatorToRocFunctionReference otherOperatorSymbol)
                        (leftNode |> expression moduleOriginLookup)
                        (rightNode |> expression moduleOriginLookup)

        Elm.Syntax.Expression.FunctionOrValue qualification name ->
            Ok
                (RocExpressionReference
                    (case moduleOriginLookup |> FastDict.get ( qualification, name ) of
                        Nothing ->
                            -- locally declared or variable
                            { moduleOrigin = Nothing
                            , name = name |> rocNameSanitize
                            }

                        Just moduleOrigin ->
                            case { moduleOrigin = moduleOrigin, name = name } |> referenceToRoc of
                                Just rocReference ->
                                    rocReference

                                Nothing ->
                                    { moduleOrigin = Nothing
                                    , name =
                                        referenceToRocName
                                            { moduleOrigin = moduleOrigin
                                            , name = name
                                            }
                                    }
                    )
                )

        Elm.Syntax.Expression.IfBlock conditionNode onTrueNode onFalseNode ->
            Result.map3
                (\condition onTrue onFalse ->
                    RocExpressionIfThenElse
                        { condition = condition
                        , onTrue = onTrue
                        , onFalse = onFalse
                        }
                )
                (conditionNode |> expression moduleOriginLookup)
                (onTrueNode |> expression moduleOriginLookup)
                (onFalseNode |> expression moduleOriginLookup)

        Elm.Syntax.Expression.ParenthesizedExpression inParens ->
            inParens |> expression moduleOriginLookup

        Elm.Syntax.Expression.Negation inNegationNode ->
            Result.map
                (\inNegation ->
                    RocExpressionCall
                        { called =
                            RocExpressionReference
                                { moduleOrigin = Just "Num", name = "neg" }
                        , argument0 = inNegation
                        , argument1Up = []
                        }
                )
                (inNegationNode |> expression moduleOriginLookup)

        Elm.Syntax.Expression.RecordAccess recordNode (Elm.Syntax.Node.Node _ fieldName) ->
            Result.map
                (\record ->
                    RocExpressionRecordAccess
                        { record = record
                        , field = fieldName |> String.replace "." ""
                        }
                )
                (recordNode |> expression moduleOriginLookup)

        Elm.Syntax.Expression.TupledExpression parts ->
            case parts of
                [] ->
                    -- invalid syntax
                    -- should be handled by Elm.Syntax.Expression.UnitExpr
                    Ok RocExpressionUnit

                [ inParens ] ->
                    -- invalid syntax
                    -- should be handled by Elm.Syntax.Expression.ParenthesizedExpression
                    expression moduleOriginLookup inParens

                [ part0Node, part1Node ] ->
                    Result.map2
                        (\part0 part1 ->
                            RocExpressionTuple { part0 = part0, part1 = part1 }
                        )
                        (part0Node |> expression moduleOriginLookup)
                        (part1Node |> expression moduleOriginLookup)

                [ part0Node, part1Node, part2Node ] ->
                    Result.map3
                        (\part0 part1 part2 ->
                            RocExpressionTriple
                                { part0 = part0, part1 = part1, part2 = part2 }
                        )
                        (part0Node |> expression moduleOriginLookup)
                        (part1Node |> expression moduleOriginLookup)
                        (part2Node |> expression moduleOriginLookup)

                _ :: _ :: _ :: _ :: _ ->
                    Err "too many tuple parts"

        Elm.Syntax.Expression.ListExpr elementNodes ->
            Result.map (\elements -> RocExpressionList elements)
                (elementNodes
                    |> listMapAndCombineOk
                        (\element -> element |> expression moduleOriginLookup)
                )

        Elm.Syntax.Expression.RecordExpr fieldNodes ->
            Result.map (\fields -> RocExpressionRecord fields)
                (fieldNodes
                    |> listMapAndCombineOk
                        (\(Elm.Syntax.Node.Node _ ( Elm.Syntax.Node.Node _ fieldName, fieldValueNode )) ->
                            Result.map
                                (\fieldValue -> ( fieldName, fieldValue ))
                                (fieldValueNode |> expression moduleOriginLookup)
                        )
                    |> Result.map FastDict.fromList
                )

        Elm.Syntax.Expression.RecordUpdateExpression (Elm.Syntax.Node.Node _ originalRecordVariable) fieldNodes ->
            Result.map
                (\fields ->
                    RocExpressionRecordUpdate
                        { originalRecordVariable =
                            referenceToRocName
                                (case moduleOriginLookup |> FastDict.get ( [], originalRecordVariable ) of
                                    Nothing ->
                                        { moduleOrigin = []
                                        , name = originalRecordVariable
                                        }

                                    Just moduleOrigin ->
                                        { moduleOrigin = moduleOrigin
                                        , name = originalRecordVariable
                                        }
                                )
                        , fields = fields
                        }
                )
                (fieldNodes
                    |> listMapAndCombineOk
                        (\(Elm.Syntax.Node.Node _ ( Elm.Syntax.Node.Node _ fieldName, fieldValueNode )) ->
                            Result.map
                                (\fieldValue -> ( fieldName, fieldValue ))
                                (fieldValueNode |> expression moduleOriginLookup)
                        )
                    |> Result.map FastDict.fromList
                )

        Elm.Syntax.Expression.LambdaExpression lambda ->
            case lambda.args of
                [] ->
                    Err "lambda without parameters is invalid syntax"

                parameter0Node :: parameter1UpNodes ->
                    Result.map3
                        (\parameter0 parameter1Up result ->
                            RocExpressionLambda
                                { parameter0 = parameter0
                                , parameter1Up = parameter1Up
                                , result = result
                                }
                        )
                        (parameter0Node |> pattern moduleOriginLookup)
                        (parameter1UpNodes
                            |> listMapAndCombineOk
                                (\parameter ->
                                    parameter |> pattern moduleOriginLookup
                                )
                        )
                        (lambda.expression |> expression moduleOriginLookup)

        Elm.Syntax.Expression.CaseExpression caseOf ->
            case caseOf.cases of
                [] ->
                    Err "case-of without cases invalid syntax"

                case0Node :: case1Node ->
                    Result.map3
                        (\matched case0 case1Up ->
                            RocExpressionWhenIs
                                { matched = matched
                                , case0 = case0
                                , case1Up = case1Up
                                }
                        )
                        (caseOf.expression |> expression moduleOriginLookup)
                        (case0Node |> case_ moduleOriginLookup)
                        (case1Node
                            |> listMapAndCombineOk
                                (\parameter ->
                                    parameter |> case_ moduleOriginLookup
                                )
                        )

        Elm.Syntax.Expression.LetExpression letIn ->
            case letIn.declarations of
                [] ->
                    Err "let-in without declarations is invalid syntax"

                declaration0Node :: declaration1UpNode ->
                    Result.map3
                        (\declaration0 declaration1Up result ->
                            let
                                combinedFurtherVariableDestructurings : List { pattern : RocPattern, variable : String }
                                combinedFurtherVariableDestructurings =
                                    declaration0.furtherVariableDestructurings
                                        ++ (declaration1Up
                                                |> List.concatMap .furtherVariableDestructurings
                                           )
                            in
                            RocExpressionWithLocalDeclarations
                                { declaration0 = declaration0.main
                                , declaration1Up =
                                    (declaration1Up
                                        |> List.map .main
                                    )
                                        ++ (combinedFurtherVariableDestructurings
                                                |> List.map
                                                    (\furtherDestructuring ->
                                                        RocLocalDestructuring
                                                            { pattern = furtherDestructuring.pattern
                                                            , expression =
                                                                RocExpressionReference
                                                                    { moduleOrigin = Nothing
                                                                    , name = furtherDestructuring.variable
                                                                    }
                                                            }
                                                    )
                                           )
                                , result = result
                                }
                        )
                        -- destructuring patterns currently do not accept as
                        -- https://github.com/roc-lang/roc/issues/7328
                        (declaration0Node |> letDeclaration moduleOriginLookup)
                        (declaration1UpNode
                            |> listMapAndCombineOk
                                (\parameter ->
                                    parameter |> letDeclaration moduleOriginLookup
                                )
                        )
                        (letIn.expression |> expression moduleOriginLookup)


rocExpressionIsDefinitelyOfTypeString : RocExpression -> Bool
rocExpressionIsDefinitelyOfTypeString rocExpression =
    case rocExpression of
        RocExpressionString _ ->
            True

        RocExpressionCall call ->
            call.called
                == RocExpressionReference { moduleOrigin = Just "Str", name = "concat" }
                && ((call.argument1Up |> List.length) == 1)

        RocExpressionUnit ->
            False

        RocExpressionInteger _ ->
            False

        RocExpressionF64 _ ->
            False

        RocExpressionReference _ ->
            False

        RocExpressionRecordAccessFunction _ ->
            False

        RocExpressionRecordAccess _ ->
            False

        RocExpressionTuple _ ->
            False

        RocExpressionTriple _ ->
            False

        RocExpressionIfThenElse _ ->
            False

        RocExpressionList _ ->
            False

        RocExpressionRecord _ ->
            False

        RocExpressionRecordUpdate _ ->
            False

        RocExpressionLambda _ ->
            False

        RocExpressionWhenIs _ ->
            False

        RocExpressionWithLocalDeclarations _ ->
            False


case_ :
    ModuleOriginLookup
    -> Elm.Syntax.Expression.Case
    ->
        Result
            String
            { pattern : RocPattern, result : RocExpression }
case_ moduleOriginLookup ( patternNode, resultNode ) =
    Result.map2
        (\casePattern result ->
            { pattern = casePattern, result = result }
        )
        (patternNode |> pattern moduleOriginLookup)
        (resultNode |> expression moduleOriginLookup)


letDeclaration :
    ModuleOriginLookup
    -> Elm.Syntax.Node.Node Elm.Syntax.Expression.LetDeclaration
    ->
        Result
            String
            { main : RocLocalDeclaration
            , furtherVariableDestructurings :
                List { pattern : RocPattern, variable : String }
            }
letDeclaration moduleOriginLookup (Elm.Syntax.Node.Node _ syntaxLetDeclaration) =
    case syntaxLetDeclaration of
        Elm.Syntax.Expression.LetDestructuring destructuringPatternNode destructuringExpressionNode ->
            Result.map2
                (\destructuringPattern destructuringExpression ->
                    let
                        destructuringPatternExtractedAs :
                            { main : RocPattern
                            , furtherVariableDestructurings :
                                List { pattern : RocPattern, variable : String }
                            }
                        destructuringPatternExtractedAs =
                            destructuringPattern
                                |> destructuringPatternExtractAs
                    in
                    { main =
                        RocLocalDestructuring
                            { pattern = destructuringPatternExtractedAs.main
                            , expression = destructuringExpression
                            }
                    , furtherVariableDestructurings =
                        destructuringPatternExtractedAs.furtherVariableDestructurings
                    }
                )
                (destructuringPatternNode |> pattern moduleOriginLookup)
                (destructuringExpressionNode |> expression moduleOriginLookup)

        Elm.Syntax.Expression.LetFunction localValueOrFunction ->
            Result.map
                (\valueOrFunction ->
                    { main =
                        RocLocalDeclarationValueOrFunction
                            { name = valueOrFunction.name |> rocNameSanitize
                            , expression = valueOrFunction.expression
                            }
                    , furtherVariableDestructurings = []
                    }
                )
                (localValueOrFunction.declaration
                    |> Elm.Syntax.Node.value
                    |> valueOrFunctionDeclaration moduleOriginLookup
                )


destructuringPatternExtractAs :
    RocPattern
    ->
        { main : RocPattern
        , furtherVariableDestructurings :
            List { pattern : RocPattern, variable : String }
        }
destructuringPatternExtractAs rocPattern =
    case rocPattern of
        RocPatternIgnore ->
            { main = RocPatternIgnore, furtherVariableDestructurings = [] }

        RocPatternInteger intValue ->
            { main = RocPatternInteger intValue, furtherVariableDestructurings = [] }

        RocPatternString stringValue ->
            { main = RocPatternString stringValue, furtherVariableDestructurings = [] }

        RocPatternVariable name ->
            { main = RocPatternVariable name, furtherVariableDestructurings = [] }

        RocPatternRecord fieldNames ->
            { main = RocPatternRecord fieldNames, furtherVariableDestructurings = [] }

        RocPatternTuple parts ->
            let
                part0ExtractedAs :
                    { main : RocPattern
                    , furtherVariableDestructurings :
                        List { pattern : RocPattern, variable : String }
                    }
                part0ExtractedAs =
                    parts.part0 |> destructuringPatternExtractAs

                part1ExtractedAs :
                    { main : RocPattern
                    , furtherVariableDestructurings :
                        List { pattern : RocPattern, variable : String }
                    }
                part1ExtractedAs =
                    parts.part1 |> destructuringPatternExtractAs
            in
            { main =
                RocPatternTuple
                    { part0 = part0ExtractedAs.main
                    , part1 = part1ExtractedAs.main
                    }
            , furtherVariableDestructurings =
                part0ExtractedAs.furtherVariableDestructurings
                    ++ part1ExtractedAs.furtherVariableDestructurings
            }

        RocPatternTriple parts ->
            let
                part0ExtractedAs :
                    { main : RocPattern
                    , furtherVariableDestructurings :
                        List { pattern : RocPattern, variable : String }
                    }
                part0ExtractedAs =
                    parts.part0 |> destructuringPatternExtractAs

                part1ExtractedAs :
                    { main : RocPattern
                    , furtherVariableDestructurings :
                        List { pattern : RocPattern, variable : String }
                    }
                part1ExtractedAs =
                    parts.part1 |> destructuringPatternExtractAs

                part2ExtractedAs :
                    { main : RocPattern
                    , furtherVariableDestructurings :
                        List { pattern : RocPattern, variable : String }
                    }
                part2ExtractedAs =
                    parts.part2 |> destructuringPatternExtractAs
            in
            { main =
                RocPatternTriple
                    { part0 = part0ExtractedAs.main
                    , part1 = part1ExtractedAs.main
                    , part2 = part2ExtractedAs.main
                    }
            , furtherVariableDestructurings =
                part0ExtractedAs.furtherVariableDestructurings
                    ++ part1ExtractedAs.furtherVariableDestructurings
            }

        RocPatternListExact elements ->
            let
                elementsExtractedAs :
                    List
                        { main : RocPattern
                        , furtherVariableDestructurings :
                            List { pattern : RocPattern, variable : String }
                        }
                elementsExtractedAs =
                    elements
                        |> List.map destructuringPatternExtractAs
            in
            { main =
                RocPatternListExact
                    (elementsExtractedAs
                        |> List.map .main
                    )
            , furtherVariableDestructurings =
                elementsExtractedAs
                    |> List.concatMap .furtherVariableDestructurings
            }

        RocPatternTagged rocPatternTagged ->
            let
                parametersExtractedAs :
                    List
                        { main : RocPattern
                        , furtherVariableDestructurings :
                            List { pattern : RocPattern, variable : String }
                        }
                parametersExtractedAs =
                    rocPatternTagged.parameters
                        |> List.map destructuringPatternExtractAs
            in
            { main =
                RocPatternTagged
                    { name = rocPatternTagged.name
                    , parameters =
                        parametersExtractedAs
                            |> List.map .main
                    }
            , furtherVariableDestructurings =
                parametersExtractedAs
                    |> List.concatMap .furtherVariableDestructurings
            }

        RocPatternListCons rocPatternListCons ->
            let
                initialElement0ExtractedAs :
                    { main : RocPattern
                    , furtherVariableDestructurings :
                        List { pattern : RocPattern, variable : String }
                    }
                initialElement0ExtractedAs =
                    rocPatternListCons.initialElement0
                        |> destructuringPatternExtractAs

                initialElement1UpExtractedAs :
                    List
                        { main : RocPattern
                        , furtherVariableDestructurings :
                            List { pattern : RocPattern, variable : String }
                        }
                initialElement1UpExtractedAs =
                    rocPatternListCons.initialElement1Up
                        |> List.map destructuringPatternExtractAs
            in
            { main =
                RocPatternListCons
                    { initialElement0 = initialElement0ExtractedAs.main
                    , initialElement1Up =
                        initialElement1UpExtractedAs
                            |> List.map .main
                    , tailVariable = rocPatternListCons.tailVariable
                    }
            , furtherVariableDestructurings =
                initialElement0ExtractedAs.furtherVariableDestructurings
                    ++ (initialElement1UpExtractedAs
                            |> List.concatMap .furtherVariableDestructurings
                       )
            }

        RocPatternAs rocPatternAs ->
            { main = RocPatternVariable rocPatternAs.variable
            , furtherVariableDestructurings = [ rocPatternAs ]
            }


expressionOperatorToRocFunctionReference :
    String
    -> Result String { moduleOrigin : Maybe String, name : String }
expressionOperatorToRocFunctionReference operatorSmbol =
    case operatorSmbol of
        "+" ->
            Ok { moduleOrigin = Just "Num", name = "add" }

        "-" ->
            Ok { moduleOrigin = Just "Num", name = "sub" }

        "*" ->
            Ok { moduleOrigin = Just "Num", name = "mul" }

        "/" ->
            Ok { moduleOrigin = Just "Num", name = "div" }

        "//" ->
            Ok { moduleOrigin = Just "Num", name = "divTrunc" }

        "^" ->
            Ok { moduleOrigin = Just "Num", name = "pow" }

        "==" ->
            Ok { moduleOrigin = Just "Bool", name = "isEq" }

        "/=" ->
            Ok { moduleOrigin = Just "Bool", name = "isNotEq" }

        "||" ->
            Ok { moduleOrigin = Just "Bool", name = "or" }

        "&&" ->
            Ok { moduleOrigin = Just "Bool", name = "and" }

        "<" ->
            Ok { moduleOrigin = Just "Num", name = "isLt" }

        ">" ->
            Ok { moduleOrigin = Just "Num", name = "isGt" }

        "<=" ->
            Ok { moduleOrigin = Just "Num", name = "isLte" }

        ">=" ->
            Ok { moduleOrigin = Just "Num", name = "isGte" }

        "::" ->
            Ok { moduleOrigin = Nothing, name = "listCons" }

        "++" ->
            Ok { moduleOrigin = Just "List", name = "concat" }

        unknownOrUnsupportedOperator ->
            Err ("unknown/unsupported operator " ++ unknownOrUnsupportedOperator)


{-| Print a roc value/function declaration
-}
printRocDeclarationValueOrFunction :
    { name : String
    , expression : RocExpression
    }
    -> Print
printRocDeclarationValueOrFunction implementation =
    Print.exactly (implementation.name ++ " =")
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.linebreakIndented
                    |> Print.followedBy
                        (implementation.expression |> printRocExpressionNotParenthesized)
                )
            )


qualifiedReferenceToRocName :
    { moduleOrigin : Maybe String
    , name : String
    }
    -> String
qualifiedReferenceToRocName reference =
    case reference.moduleOrigin of
        Nothing ->
            reference.name

        Just moduleOrigin ->
            moduleOrigin
                ++ "."
                ++ reference.name


printRocExpressionParenthesizedIfSpaceSeparated : RocExpression -> Print
printRocExpressionParenthesizedIfSpaceSeparated rocExpression =
    if rocExpression |> rocExpressionIsSpaceSeparated then
        printParenthesized (printRocExpressionNotParenthesized rocExpression)

    else
        printRocExpressionNotParenthesized rocExpression


rocExpressionIsSpaceSeparated : RocExpression -> Bool
rocExpressionIsSpaceSeparated rocExpression =
    case rocExpression of
        RocExpressionUnit ->
            False

        RocExpressionInteger _ ->
            False

        RocExpressionF64 _ ->
            False

        RocExpressionString _ ->
            False

        RocExpressionReference _ ->
            False

        RocExpressionRecordAccessFunction _ ->
            False

        RocExpressionRecordAccess _ ->
            False

        RocExpressionTuple _ ->
            False

        RocExpressionTriple _ ->
            False

        RocExpressionIfThenElse _ ->
            True

        RocExpressionList _ ->
            False

        RocExpressionRecord _ ->
            False

        RocExpressionRecordUpdate _ ->
            False

        RocExpressionCall _ ->
            True

        RocExpressionLambda _ ->
            True

        RocExpressionWhenIs _ ->
            True

        RocExpressionWithLocalDeclarations _ ->
            True


{-| Print a [`RocExpression`](#RocExpression)
-}
printRocExpressionNotParenthesized : RocExpression -> Print
printRocExpressionNotParenthesized rocExpression =
    -- IGNORE TCO
    case rocExpression of
        RocExpressionUnit ->
            Print.exactly "{}"

        RocExpressionCall call ->
            printRocExpressionNotParenthesized call.called
                |> Print.followedBy
                    (Print.withIndentAtNextMultipleOf4
                        (Print.linebreakIndented
                            |> Print.followedBy
                                ((call.argument0 :: call.argument1Up)
                                    |> Print.listMapAndIntersperseAndFlatten
                                        (\argument ->
                                            printRocExpressionParenthesizedIfSpaceSeparated
                                                argument
                                        )
                                        Print.linebreakIndented
                                )
                        )
                    )

        RocExpressionReference reference ->
            Print.exactly
                (reference |> qualifiedReferenceToRocName)

        RocExpressionIfThenElse ifThenElse ->
            printRocExpressionIfThenElse ifThenElse

        RocExpressionInteger int ->
            Print.exactly (String.fromInt int)

        RocExpressionF64 float ->
            Print.exactly (floatLiteral float)

        RocExpressionString string ->
            stringLiteral string

        RocExpressionRecordAccessFunction fieldName ->
            Print.exactly ("." ++ (fieldName |> String.replace "." ""))

        RocExpressionTuple parts ->
            Print.exactly "( "
                |> Print.followedBy
                    ([ parts.part0, parts.part1 ]
                        |> Print.listMapAndIntersperseAndFlatten
                            printRocExpressionNotParenthesized
                            (Print.linebreakIndented
                                |> Print.followedBy
                                    (Print.exactly ", ")
                            )
                    )
                |> Print.followedBy Print.linebreakIndented
                |> Print.followedBy (Print.exactly ")")

        RocExpressionTriple parts ->
            Print.exactly "( "
                |> Print.followedBy
                    ([ parts.part0, parts.part1, parts.part2 ]
                        |> Print.listMapAndIntersperseAndFlatten
                            printRocExpressionNotParenthesized
                            (Print.linebreakIndented
                                |> Print.followedBy
                                    (Print.exactly ", ")
                            )
                    )
                |> Print.followedBy Print.linebreakIndented
                |> Print.followedBy (Print.exactly ")")

        RocExpressionWithLocalDeclarations expressionWithLocalDeclarations ->
            printRocExpressionWithLocalDeclarations expressionWithLocalDeclarations

        RocExpressionWhenIs syntaxWhenIs ->
            printRocExpressionWhenIs syntaxWhenIs

        RocExpressionLambda syntaxLambda ->
            printRocExpressionLambda syntaxLambda

        RocExpressionRecord fields ->
            printRocExpressionRecord fields

        RocExpressionList elements ->
            printRocExpressionList elements

        RocExpressionRecordAccess syntaxRecordAccess ->
            printRocExpressionParenthesizedIfSpaceSeparated
                syntaxRecordAccess.record
                |> Print.followedBy
                    (Print.exactly
                        ("." ++ (syntaxRecordAccess.field |> String.replace "." ""))
                    )

        RocExpressionRecordUpdate syntaxRecordUpdate ->
            printRocExpressionRecordUpdate syntaxRecordUpdate


floatLiteral : Float -> String
floatLiteral float =
    if (float |> Basics.truncate |> Basics.toFloat) == float then
        String.fromFloat float ++ ".0_f64"

    else
        String.fromFloat float ++ "_f64"


printRocExpressionList : List RocExpression -> Print
printRocExpressionList listElements =
    case listElements of
        [] ->
            Print.exactly "[]"

        element0 :: element1Up ->
            printExactlySquareOpeningSpace
                |> Print.followedBy
                    ((element0 :: element1Up)
                        |> Print.listMapAndIntersperseAndFlatten
                            (\element ->
                                Print.withIndentIncreasedBy 2
                                    (element |> printRocExpressionNotParenthesized)
                            )
                            (Print.linebreakIndented
                                |> Print.followedBy (Print.exactly ", ")
                            )
                    )
                |> Print.followedBy printExactlySquareClosing


printRocExpressionRecordUpdate :
    { originalRecordVariable : String
    , fields : FastDict.Dict String RocExpression
    }
    -> Print
printRocExpressionRecordUpdate syntaxRecordUpdate =
    printExactlyCurlyOpeningSpace
        |> Print.followedBy
            (Print.withIndentIncreasedBy 2
                (Print.exactly syntaxRecordUpdate.originalRecordVariable)
            )
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.linebreakIndented
                    |> Print.followedBy (Print.exactly "& ")
                    |> Print.followedBy
                        (syntaxRecordUpdate.fields
                            |> FastDict.toList
                            |> Print.listMapAndIntersperseAndFlatten
                                (\( fieldName, fieldValue ) ->
                                    Print.withIndentIncreasedBy 2
                                        (Print.exactly (fieldName ++ ":"))
                                        |> Print.followedBy
                                            (Print.withIndentAtNextMultipleOf4
                                                (Print.linebreakIndented
                                                    |> Print.followedBy
                                                        (printRocExpressionNotParenthesized fieldValue)
                                                )
                                            )
                                )
                                (Print.linebreakIndented
                                    |> Print.followedBy
                                        (Print.exactly ", ")
                                )
                        )
                )
            )
        |> Print.followedBy Print.linebreakIndented
        |> Print.followedBy printExactlyCurlyClosing


printRocExpressionLambda :
    { parameter0 : RocPattern
    , parameter1Up : List RocPattern
    , result : RocExpression
    }
    -> Print
printRocExpressionLambda syntaxLambda =
    printExactlyBackSlash
        |> Print.followedBy
            ((syntaxLambda.parameter0 :: syntaxLambda.parameter1Up)
                |> Print.listMapAndIntersperseAndFlatten
                    (\parameter ->
                        -- https://github.com/roc-lang/roc/issues/7325
                        case parameter of
                            RocPatternAs parameterAs ->
                                printParenthesized
                                    (RocPatternAs parameterAs
                                        |> printRocPatternNotParenthesized
                                    )

                            parameterNotAs ->
                                parameterNotAs |> printRocPatternNotParenthesized
                    )
                    (Print.exactly ", ")
            )
        |> Print.followedBy (Print.exactly " ->")
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.linebreakIndented
                    |> Print.followedBy
                        (printRocExpressionNotParenthesized syntaxLambda.result)
                )
            )


printRocExpressionIfThenElse :
    { condition : RocExpression
    , onTrue : RocExpression
    , onFalse : RocExpression
    }
    -> Print
printRocExpressionIfThenElse syntaxIfThenElse =
    printExactlyIf
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.linebreakIndented
                    |> Print.followedBy
                        (printRocExpressionNotParenthesized syntaxIfThenElse.condition)
                )
            )
        |> Print.followedBy Print.linebreakIndented
        |> Print.followedBy (Print.exactly "then")
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.linebreakIndented
                    |> Print.followedBy
                        (printRocExpressionNotParenthesized syntaxIfThenElse.onTrue)
                    |> Print.followedBy Print.linebreak
                )
            )
        |> Print.followedBy Print.linebreakIndented
        |> Print.followedBy printExactlyElse
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.linebreakIndented
                    |> Print.followedBy
                        (printRocExpressionNotParenthesized syntaxIfThenElse.onFalse)
                )
            )


printRocExpressionWhenIs :
    { matched : RocExpression
    , case0 : { pattern : RocPattern, result : RocExpression }
    , case1Up : List { pattern : RocPattern, result : RocExpression }
    }
    -> Print
printRocExpressionWhenIs whenIs =
    printExactlyWhen
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.linebreakIndented
                    |> Print.followedBy
                        (printRocExpressionNotParenthesized
                            whenIs.matched
                        )
                )
            )
        |> Print.followedBy Print.linebreakIndented
        |> Print.followedBy (Print.exactly "is")
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.linebreakIndented
                    |> Print.followedBy
                        ((whenIs.case0 :: whenIs.case1Up)
                            |> Print.listMapAndIntersperseAndFlatten
                                (\syntaxCase -> syntaxCase |> printRocExpressionWhenIsCase)
                                printLinebreakLinebreakIndented
                        )
                )
            )


printRocExpressionWithLocalDeclarations :
    { declaration0 : RocLocalDeclaration
    , declaration1Up : List RocLocalDeclaration
    , result : RocExpression
    }
    -> Print
printRocExpressionWithLocalDeclarations syntaxLetIn =
    (syntaxLetIn.declaration0 :: syntaxLetIn.declaration1Up)
        |> Print.listMapAndIntersperseAndFlatten
            (\localDecl ->
                localDecl
                    |> printRocLocalDeclaration
            )
            (Print.linebreakIndented
                |> Print.followedBy Print.linebreakIndented
            )
        |> Print.followedBy printLinebreakLinebreakIndented
        |> Print.followedBy
            (printRocExpressionNotParenthesized syntaxLetIn.result)


printRocLocalDeclaration : RocLocalDeclaration -> Print
printRocLocalDeclaration localDeclaration =
    case localDeclaration of
        RocLocalDeclarationValueOrFunction letDeclarationExpression ->
            printRocDeclarationValueOrFunction letDeclarationExpression

        RocLocalDestructuring localDestructuring ->
            printParenthesized
                (printRocPatternNotParenthesized localDestructuring.pattern)
                |> Print.followedBy (Print.exactly " =")
                |> Print.followedBy
                    (Print.withIndentAtNextMultipleOf4
                        (Print.linebreakIndented
                            |> Print.followedBy
                                (printRocExpressionNotParenthesized localDestructuring.expression)
                        )
                    )


printRocExpressionWhenIsCase :
    { pattern : RocPattern, result : RocExpression }
    -> Print
printRocExpressionWhenIsCase branch =
    printRocPatternNotParenthesized branch.pattern
        |> Print.followedBy (Print.exactly " ->")
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.linebreakIndented
                    |> Print.followedBy
                        (printRocExpressionNotParenthesized branch.result)
                )
            )


{-| Print value/function declarations into a roc module that exposes all members.
Will also add some internal wrapper declarations.
-}
rocDeclarationsToModuleString :
    List
        { name : String
        , expression : RocExpression
        }
    -> String
rocDeclarationsToModuleString declarations =
    Print.exactly
        ("""module [
    """
            ++ (declarations
                    |> List.map (\dec -> dec.name)
                    |> String.join ",\n    "
               )
            ++ """
]


basicsIdentity =
    \\a -> a

basicsAlways =
    \\a, _ -> a

basicsRemainderBy =
    \\divisor, toDivide -> Num.rem toDivide divisor

basicsModBy =
    \\divisor, toDivide ->
        remainder = Num.rem toDivide divisor

        if ((remainder > 0 && divisor < 0) || (remainder < 0 && divisor > 0)) then
            remainder + toDivide
        
        else
            remainder

stringJoin =
    \\separator, strings -> Str.joinWith strings separator

stringSplit =
    \\separator, string -> Str.splitOn string separator

stringRepeat =
    \\toRepeat, howOften -> Str.repeat toRepeat howOften

stringStartsWith =
    \\string, startToTestFor -> Str.startsWith string startToTestFor

stringEndsWith =
    \\string, startToTestFor -> Str.endsWith string startToTestFor

stringReplace =
    \\string, toReplace, replacement -> Str.replaceEach string toReplace replacement

stringContains =
    \\substring, string -> Str.contains string substring

stringAny =
    \\isNeedle, string -> List.any (Str.toUtf8 string) isNeedle

stringAll =
    \\isNeedle, string -> List.all (Str.toUtf8 string) isNeedle

stringFromChar =
    \\char ->
        when Str.fromUtf8 [ char ] is
            Err _ ->
                ""
            
            Ok charAsString ->
                charAsString

stringCons =
    \\headChar, tailString ->
        Str.concat (stringFromChar headChar) tailString

stringSlice =
    \\startIndex, endIndexExclusive, string ->
        realStartIndex =
            if Num.isNegative startIndex then
                Str.countUtf8Bytes string + startIndex
            
            else
                startIndex
        
        realEndIndexExclusive =
            if Num.isNegative endIndexExclusive then
                Str.countUtf8Bytes string + endIndexExclusive
            
            else
                endIndexExclusive
        
        List.sublist
            (Str.toUtf8 string)
            { start: startIndex,
              len: realEndIndexExclusive - 1 - startIndex
            }

stringToInt =
    \\string -> rocResultToMaybe (Str.toI64 string)

stringToFloat =
    \\string -> rocResultToMaybe (Str.toF64 string)

listCons =
    \\head, tail -> List.prepend tail head

listRepeat =
    \\howOften, element -> List.repeat element howOften

listAny =
    \\isNeedle, list -> List.any list isNeedle

listAll =
    \\isRegular, list -> List.all list isRegular

listFilter =
    \\isOkay, list -> List.keepIf list isOkay

listMap =
    \\elementChange, list -> List.map list elementChange

listMap2 =
    \\abCombine, aList, bList -> List.map2 aList bList abCombine

listMap3 =
    \\abcCombine, aList, bList, cList -> List.map3 aList bList cList abcCombine

listMap4 =
    \\abcdCombine, aList, bList, cList, dList -> List.map4 aList bList cList dList abcdCombine

listSortWith =
    \\elementCompare, list -> List.sortWith list elementCompare

listRange =
    \\startInclusive, endInclusive ->
        List.range { start: At startInclusive, end: At endInclusive }

listTake =
    \\howMany, list -> List.takeFirst list howMany

listDrop =
    \\howMany, list -> List.dropFirst list howMany

listConcatMap =
    \\elementToList, list -> List.joinMap list elementToList

listIntersperse =
    \\inBetweenElement, list -> List.intersperse list inBetweenElement

listFoldl =
    \\reduce, initialFolded, list ->
        List.walk list initialFolded (\\soFar, element -> reduce element soFar)

listFoldr =
    \\reduce, initialFolded, list ->
        List.walkBackwards list initialFolded (\\soFar, element -> reduce element soFar)

stringToLower =
    \\string ->
        when Str.fromUtf8 (List.map (Str.toUtf8 string) charToLower) is
            Ok changed ->
                changed
            
            Err _ ->
                string

stringToUpper =
    \\string ->
        when Str.fromUtf8 (List.map (Str.toUtf8 string) charToLower) is
            Ok changed ->
                changed
            
            Err _ ->
                string

charToLower =
    \\char ->
        # only basic latin letters for now
        if char >= 'A' && char <= 'Z' then
            'a' + (char - 'A')
        
        else
            char

charToUpper =
    \\char ->
        # only basic latin letters for now
        if char >= 'a' && char <= 'z' then
            'A' + (char - 'a')
        
        else
            char

dictMap =
    \\keyValueToNewValue, dict -> Dict.map dict keyValueToNewValue

dictFoldl =
    \\reduce, initialFolded, dict ->
        Dict.walk dict initialFolded (\\soFar, k, v -> reduce k v soFar)

dictFoldr =
    \\reduce, initialFolded, dict ->
        Dict.walkBackwards dict initialFolded (\\soFar, k, v -> reduce k v soFar)

dictGet =
    \\key, dict -> Dict.map dict key

dictMember =
    \\key, dict -> Dict.contains dict key

dictInsert =
    \\key, value, dict -> Dict.remove dict key value

dictRemove =
    \\key, dict -> Dict.remove dict key

dictUnion =
    \\newDict, baseDict -> Dict.insertAll baseDict newDict

dictFilter =
    \\isOkay, dict ->
        Dict.keepIf dict (\\( k, v ) -> isOkay k v)

rocResultToMaybe =
    \\result ->
        when result is
            Err _ ->
                Maybe_Nothing
            
            Ok success ->
                Maybe_Just success

"""
        )
        |> Print.FollowedBy
            (declarations
                |> Print.listMapAndIntersperseAndFlatten
                    printRocDeclarationValueOrFunction
                    (Print.linebreak
                        |> Print.FollowedBy Print.linebreak
                    )
            )
        |> Print.FollowedBy Print.linebreak
        |> Print.toString


listMapAndCombineOk : (a -> Result err ok) -> List a -> Result err (List ok)
listMapAndCombineOk elementToResult list =
    listMapAndCombineOkFrom [] elementToResult list


listMapAndCombineOkFrom : List ok -> (a -> Result err ok) -> List a -> Result err (List ok)
listMapAndCombineOkFrom soFar elementToResult list =
    case list of
        [] ->
            Ok (soFar |> List.reverse)

        head :: tail ->
            case head |> elementToResult of
                Err headErr ->
                    Err headErr

                Ok headOk ->
                    listMapAndCombineOkFrom (headOk :: soFar)
                        elementToResult
                        tail


resultAndThen3 :
    (a -> b -> c -> Result error d)
    -> Result error a
    -> Result error b
    -> Result error c
    -> Result error d
resultAndThen3 abToResult aResult bResult cResult =
    case aResult of
        Err error ->
            Err error

        Ok a ->
            case bResult of
                Err error ->
                    Err error

                Ok b ->
                    case cResult of
                        Err error ->
                            Err error

                        Ok c ->
                            abToResult a b c


printLinebreakLinebreakIndented : Print.Print
printLinebreakLinebreakIndented =
    Print.linebreak
        |> Print.followedBy Print.linebreakIndented


printExactlyCommaSpace : Print.Print
printExactlyCommaSpace =
    Print.exactly ", "


printExactlySquareOpeningSpace : Print.Print
printExactlySquareOpeningSpace =
    Print.exactly "[ "


printExactlyCurlyOpeningSpace : Print.Print
printExactlyCurlyOpeningSpace =
    Print.exactly "{ "


printExactlyParensOpening : Print
printExactlyParensOpening =
    Print.exactly "("


printExactlyParensClosing : Print
printExactlyParensClosing =
    Print.exactly ")"


printExactlySquareClosing : Print
printExactlySquareClosing =
    Print.exactly "]"


printExactlyCurlyClosing : Print
printExactlyCurlyClosing =
    Print.exactly "}"


printExactlyBackSlash : Print
printExactlyBackSlash =
    Print.exactly "\\"


printExactlyUnderscore : Print
printExactlyUnderscore =
    Print.exactly "_"


printExactlyWhen : Print
printExactlyWhen =
    Print.exactly "when"


printExactlyIf : Print
printExactlyIf =
    Print.exactly "if"


printExactlyElse : Print
printExactlyElse =
    Print.exactly "else"
