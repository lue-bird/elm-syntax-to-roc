module ElmSyntaxToRocTests exposing (suite)

import Elm.Parser
import ElmSyntaxToRoc
import Expect
import Test exposing (Test)


suite : Test
suite =
    Test.describe "elm-syntax-to-roc"
        [ Test.test ":: with multiple initial elements and final tail variable"
            (\() ->
                """module A exposing (..)
a0 =
    case [] of
        b :: Just c {- 0 -} {- 1 -} :: (d) ->
            b

        _ ->
            0

a1 =
    case [] of
        b :: Just c {- 0 -} {- 1 -} :: (_) ->
            b

        _ ->
            0

a2 x =
    case x of
        (({y,z}::tail), Maybe.Nothing as nothing, (Just[""],0)) ->
            0
        _ ->
            1
"""
                    |> expectTranspiledToRocStringAs
                        """module [
    aA0,
    aA1,
    aA2
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

aA0 =
    when [] is
        [ b, MaybeJust c, .. as d ] ->
            b

        _ ->
            0

aA1 =
    when [] is
        [ b, MaybeJust c, .. ] ->
            b

        _ ->
            0

aA2 =
    \\x ->
        when x is
            ( [ { y, z }, .. as tail ], (MaybeNothing) as nothing, ( MaybeJust [ "" ], 0 ) ) ->
                0

            _ ->
                1
"""
            )
        ]


expectTranspiledToRocStringAs : String -> String -> Expect.Expectation
expectTranspiledToRocStringAs expected source =
    case
        [ source, elmCoreMaybeSourcePartial ]
            |> List.foldl
                (\moduleSource soFarOrError ->
                    case moduleSource |> Elm.Parser.parseToFile of
                        Err deadEnds ->
                            Err
                                (("failed to parse actual source: "
                                    ++ (deadEnds |> Debug.toString)
                                 )
                                    :: (case soFarOrError of
                                            Err errors ->
                                                errors

                                            Ok _ ->
                                                []
                                       )
                                )

                        Ok parsed ->
                            case soFarOrError of
                                Err error ->
                                    Err error

                                Ok soFar ->
                                    Ok (parsed :: soFar)
                )
                (Ok [])
    of
        Err deadEnds ->
            Expect.fail
                ("failed to parse actual source: "
                    ++ (deadEnds |> Debug.toString)
                )

        Ok parsedModules ->
            let
                transpiledResult :
                    { errors : List String
                    , declarations :
                        List { name : String, expression : ElmSyntaxToRoc.RocExpression }
                    }
                transpiledResult =
                    parsedModules |> ElmSyntaxToRoc.modules
            in
            case transpiledResult.errors of
                transpilationError0 :: transpilationError1Up ->
                    Expect.fail
                        ("failed to transpile the parsed elm to roc: "
                            ++ ((transpilationError0 :: transpilationError1Up)
                                    |> String.join " and "
                               )
                        )

                [] ->
                    let
                        printed : String
                        printed =
                            transpiledResult.declarations |> ElmSyntaxToRoc.rocDeclarationsToModuleString
                    in
                    if printed == expected then
                        Expect.pass

                    else
                        Expect.fail
                            ("actual printed source is\n\n"
                                ++ printed
                                ++ "\n\nbut I expected\n\n"
                                ++ expected
                                ++ "\n\nThey differ in lines\n"
                                ++ (List.map2
                                        (\actualLine expectedLine -> { actual = actualLine, expected = expectedLine })
                                        (printed |> String.lines)
                                        (expected |> String.lines)
                                        |> List.indexedMap
                                            (\i lines ->
                                                if lines.actual == lines.expected then
                                                    Nothing

                                                else
                                                    Just ((i |> String.fromInt) ++ ": " ++ lines.actual)
                                            )
                                        |> List.filterMap identity
                                        |> List.take 10
                                        |> String.join "\n"
                                   )
                            )


elmCoreMaybeSourcePartial : String
elmCoreMaybeSourcePartial =
    """module Maybe exposing
  ( Maybe(..)
  , andThen
  , map, map2, map3, map4, map5
  , withDefault
  )


import Basics exposing (Bool(..))


type Maybe a
    = Just a
    | Nothing
"""
