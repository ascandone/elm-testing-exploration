module Test.TicTacToePage exposing (suite)

import Html
import Main
import Page.TicTacToe as App exposing (Coords(..), Player(..))
import Route
import Test exposing (..)
import Test.Common exposing (changeUrl, simulationMain)
import Test.Html.Query as Query
import Test.Simulation as Simulation exposing (Simulation)


simulation : Simulation Main.Model Main.Msg (Cmd Main.Msg) {}
simulation =
    simulationMain
        |> changeUrl Route.TicTacToe


suite : Test
suite =
    describe "tic tac toe UI tests"
        [ test "clicking on a filled cell is a noop" <|
            \() ->
                simulation
                    -- X
                    |> Simulation.simulate (App.cellClick First First)
                    -- O
                    |> Simulation.simulate (App.cellClick First First)
                    |> Simulation.expectHtml
                        (\query ->
                            query
                                |> App.cellQuery First First
                                |> Query.contains [ Html.text "X" ]
                        )
                    |> Simulation.run
        , test "complete game" <|
            \() ->
                simulation
                    --X
                    |> Simulation.simulate (App.cellClick First First)
                    -- O
                    |> Simulation.simulate (App.cellClick First Second)
                    --X
                    |> Simulation.simulate (App.cellClick Second First)
                    -- O
                    |> Simulation.simulate (App.cellClick Second Second)
                    --X
                    |> Simulation.simulate (App.cellClick Third First)
                    -- O
                    |> Simulation.simulate (App.cellClick Third Second)
                    |> Simulation.expectHtml (App.expectPlayerWon X)
                    |> Simulation.run
        ]
