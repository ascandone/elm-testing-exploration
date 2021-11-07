module Test.SelectDemoPage exposing (suite)

import Application
import Common
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
import Url exposing (Url)


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
                    |> Simulation.simulate
                        ( Event.focus
                        , Query.find
                            [ Selector.attribute (Common.dataTestId "countries-select")
                            , Selector.attribute (Common.dataTestId "input")
                            ]
                        )
                    |> Simulation.expectHtml
                        (\query ->
                            query
                                |> Query.contains [ Html.text "Cambodia" ]
                        )
                    |> Simulation.simulate
                        ( Event.click
                        , Query.find
                            [ Selector.attribute (Common.dataTestId "countries-select")
                            , Selector.containing [ Selector.text "Cambodia" ]
                            ]
                        )
                    |> Simulation.run
        ]
