module Test.SelectDemoPage exposing (suite)

import Common
import Components.Select as Select
import Expect
import Html
import Html.Attributes as Attr
import Main
import Page.SelectDemo as App
import Route exposing (Route)
import Test exposing (..)
import Test.Common exposing (changeUrl, simulationMain)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import Test.Simulation as Simulation exposing (Simulation)


simulation : Simulation Main.Model Main.Msg {}
simulation =
    simulationMain
        |> changeUrl Route.SelectDemo


suite : Test
suite =
    describe "tic tac toe UI tests"
        [ test "clicking on a filled cell is a noop" <|
            \() ->
                simulation
                    |> Simulation.simulate (Select.focus App.countriesSelectTestId)
                    |> Simulation.simulate (Select.input App.countriesSelectTestId "Alb")
                    |> Simulation.simulate (Select.selectedItemWithId "Albania" App.countriesSelectTestId)
                    |> Simulation.expectHtml (Query.has [ Selector.text "1" ])
                    |> Simulation.run
        ]
