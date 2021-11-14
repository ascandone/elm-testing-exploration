module Test.List exposing (suite)

import Expect
import Fuzz
import List.Extra
import Test exposing (Test)


suite : Test
suite =
    Test.describe "List.Extra module"
        [ Test.describe "Takedrop function"
            [ Test.fuzz2 (Fuzz.list Fuzz.string) Fuzz.int "Is equal to (take, drop)" <|
                \list index ->
                    list
                        |> List.Extra.takeDrop index
                        |> Expect.equal
                            ( List.take index list
                            , List.drop index list
                            )
            , Test.test "Simple example" <|
                \() ->
                    [ "a", "b", "c", "d", "e" ]
                        |> List.Extra.takeDrop 2
                        |> Expect.equal
                            ( [ "a", "b" ]
                            , [ "c", "d", "e" ]
                            )
            ]
        , Test.describe "splitAt function"
            [ Test.test "Empty" <|
                \() ->
                    []
                        |> List.Extra.splitAt 0
                        |> Expect.equal Nothing
            , Test.test "Singleton" <|
                \() ->
                    [ "x" ]
                        |> List.Extra.splitAt 1
                        |> Expect.equal (Just ( [], "x", [] ))
            , Test.test "Start" <|
                \() ->
                    [ "a", "b", "c", "d", "e" ]
                        |> List.Extra.splitAt 0
                        |> Expect.equal
                            (Just
                                ( []
                                , "a"
                                , [ "b", "c", "d", "e" ]
                                )
                            )
            , Test.test "Middle" <|
                \() ->
                    [ "a", "b", "c", "d", "e" ]
                        |> List.Extra.splitAt 2
                        |> Expect.equal
                            (Just
                                ( [ "a", "b" ], "c", [ "d", "e" ] )
                            )
            , Test.test "End" <|
                \() ->
                    [ "a", "b", "c", "d", "e" ]
                        |> List.Extra.splitAt 4
                        |> Expect.equal
                            (Just
                                ( [ "a", "b", "c", "d" ]
                                , "e"
                                , []
                                )
                            )
            , Test.fuzz2 (Fuzz.list Fuzz.string) Fuzz.int "Invariant" <|
                \list index ->
                    list
                        |> List.Extra.splitAt index
                        |> Maybe.map (\( left, middle, right ) -> left ++ [ middle ] ++ right)
                        |> Maybe.withDefault []
                        |> Expect.equal list
            ]
        ]
