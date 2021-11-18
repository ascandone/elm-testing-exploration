module Test.UndoList exposing (suite)

import Expect
import Fuzz exposing (Fuzzer)
import Fuzz.Extra as Fuzz
import Test exposing (Test)
import UndoList exposing (UndoList)
import UndoList.Extra


suite : Test
suite =
    Test.describe "UndoList"
        [ Test.test "goto 0" <|
            \() ->
                UndoList.fresh "a"
                    |> UndoList.new "b"
                    |> UndoList.new "c"
                    |> UndoList.new "d"
                    |> UndoList.Extra.goto 0
                    |> Expect.equal (UndoList [] "a" [ "b", "c", "d" ])
        , Test.test "goto 1" <|
            \() ->
                UndoList.fresh "a"
                    |> UndoList.new "b"
                    |> UndoList.new "c"
                    |> UndoList.new "d"
                    |> UndoList.Extra.goto 1
                    |> Expect.equal (UndoList [ "a" ] "b" [ "c", "d" ])
        , Test.test "goto -1" <|
            \() ->
                UndoList.fresh "a"
                    |> UndoList.new "b"
                    |> UndoList.new "c"
                    |> UndoList.new "d"
                    |> UndoList.Extra.goto -1
                    |> Expect.equal (UndoList [] "a" [ "b", "c", "d" ])
        , Test.test "goto -3" <|
            \() ->
                UndoList.fresh "a"
                    |> UndoList.new "b"
                    |> UndoList.new "c"
                    |> UndoList.new "d"
                    |> UndoList.Extra.goto -3
                    |> Expect.equal (UndoList [] "a" [ "b", "c", "d" ])
        , Test.test "goto 2 when future is not empty" <|
            \() ->
                UndoList.fresh "a"
                    |> UndoList.new "b"
                    |> UndoList.new "c"
                    |> UndoList.new "d"
                    |> UndoList.Extra.goto 0
                    |> UndoList.Extra.goto 2
                    |> Expect.equal (UndoList [ "b", "a" ] "c" [ "d" ])
        , Test.fuzz2 (undoListFuzzer Fuzz.string) Fuzz.nonNegativeInt "Length is at most index" <|
            \undoList index ->
                undoList
                    |> UndoList.Extra.goto index
                    |> (\newUndoList ->
                            UndoList.lengthPast newUndoList
                                |> Expect.atMost index
                       )
        , Test.fuzz2 (undoListFuzzer Fuzz.string) Fuzz.negativeInt "Negative nums are truncated to zero" <|
            \undoList index ->
                UndoList.Extra.goto index undoList
                    |> Expect.equal (UndoList.Extra.goto 0 undoList)
        ]


undoListFuzzer : Fuzzer state -> Fuzzer (UndoList state)
undoListFuzzer stateFuzzer =
    Fuzz.map2 (List.foldl performOp)
        (Fuzz.map UndoList.fresh stateFuzzer)
        (Fuzz.list (operationFuzzer stateFuzzer))


type Operation state
    = New state
    | Redo
    | Undo


operationFuzzer : Fuzzer state -> Fuzzer (Operation state)
operationFuzzer stateFuzzer =
    Fuzz.frequency
        [ ( 1.0, Fuzz.constant Redo )
        , ( 1.0, Fuzz.constant Undo )
        , ( 2.0, Fuzz.map New stateFuzzer )
        ]


performOp : Operation state -> UndoList state -> UndoList state
performOp op =
    case op of
        New state ->
            UndoList.new state

        Redo ->
            UndoList.redo

        Undo ->
            UndoList.undo
