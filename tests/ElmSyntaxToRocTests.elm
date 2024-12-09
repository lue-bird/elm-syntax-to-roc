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
    a_a0,
    a_a1,
    a_a2
]


basics_identity =
    \\a -> a

basics_always =
    \\a, _ -> a

basics_remainderBy =
    \\divisor, toDivide -> Num.rem toDivide divisor

string_join =
    \\seperator, strings -> Str.joinWith strings seperator

string_split =
    \\seperator, string -> Str.splitOn string seperator

string_repeat =
    \\toRepeat, howOften -> Str.repeat toRepeat howOften

string_startsWith =
    \\string, startToTestFor -> Str.startsWith string startToTestFor

string_endsWith =
    \\string, startToTestFor -> Str.endsWith string startToTestFor

string_toInt =
    \\string -> result_toMaybe (Str.toI64 string)

string_toFloat =
    \\string -> result_toMaybe (Str.toF64 string)

result_toMaybe =
    \\result ->
        when result is
            Err _ ->
                Maybe_Nothing
            
            Ok success ->
                Maybe_Just success


a_a0 =
    when
        []
    is
        [ b, Maybe_Just c, .. as d ] ->
            b

        _ ->
            0

a_a1 =
    when
        []
    is
        [ b, Maybe_Just c, .. ] ->
            b

        _ ->
            0

a_a2 =
    \\x ->
        when
            x
        is
            ( [ { y, z }, .. as tail ], (Maybe_Nothing) as nothing, ( Maybe_Just [ "" ], 0 ) ) ->
                0

            _ ->
                1
"""
            )
        ]


expectTranspiledToRocStringAs : String -> String -> Expect.Expectation
expectTranspiledToRocStringAs expected source =
    case source |> Elm.Parser.parseToFile of
        Err deadEnds ->
            Expect.fail ("failed to parse actual source: " ++ (deadEnds |> Debug.toString))

        Ok parsed ->
            case [ parsed ] |> ElmSyntaxToRoc.modules of
                Err transpilationError ->
                    Expect.fail ("failed to transpile the parsed elm to roc: " ++ transpilationError)

                Ok transpiledDeclarations ->
                    let
                        printed : String
                        printed =
                            transpiledDeclarations |> ElmSyntaxToRoc.rocDeclarationsToModuleString
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
