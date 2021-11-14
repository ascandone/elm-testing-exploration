module Test.SelectDemoPage exposing (suite)

import Components.Select as Select
import Main
import Page.SelectDemo as App
import Route
import Test exposing (..)
import Test.Common exposing (changeUrl, simulationMain)
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import Test.Simulation as Simulation exposing (Simulation)


simulation : Simulation Main.Model Main.Msg (Cmd Main.Msg) {}
simulation =
    simulationMain
        |> changeUrl Route.SelectDemo


suite : Test
suite =
    describe "SelectDemo UI tests"
        [ test "Selected countries are showed in the counter" <|
            \() ->
                simulation
                    |> Simulation.simulate (Select.focus App.countriesSelectTestId)
                    |> Simulation.simulate (Select.input App.countriesSelectTestId "Alb")
                    |> Simulation.simulate (Select.selectedItemWithId App.countriesSelectTestId "Albania")
                    |> Simulation.expectHtml (Query.has [ Selector.text "1" ])
                    |> Simulation.run
        , test "Removed countries are removed from the counter" <|
            \() ->
                simulation
                    |> Simulation.simulate (Select.focus App.countriesSelectTestId)
                    |> Simulation.simulate (Select.input App.countriesSelectTestId "Alb")
                    |> Simulation.simulate (Select.selectedItemWithId App.countriesSelectTestId "Albania")
                    |> Simulation.simulate (Select.deleteItem App.countriesSelectTestId "Albania")
                    |> Simulation.expectHtml (Query.has [ Selector.text "0" ])
                    |> Simulation.run
        ]
