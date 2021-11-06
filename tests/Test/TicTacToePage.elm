module Test.TicTacToePage exposing (suite)

import Application
import Expect
import Html
import Html.Attributes as Attr
import Main
import Page.TicTacToe as App exposing (Coords(..), Player(..))
import Route exposing (Route)
import Test exposing (..)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import Test.Simulation as Simulation exposing (Simulation)
import Url exposing (Url)


simulation : Simulation App.Model App.Msg {}
simulation =
    Simulation.fromSandbox
        { init = App.init
        , update = App.update
        , view = App.view
        }


routeToUrl : Route -> Url
routeToUrl route =
    { protocol = Url.Http
    , host = "example.com"
    , port_ = Nothing
    , path = Route.toString route
    , query = Nothing
    , fragment = Nothing
    }


simulationMain : Simulation Main.Model Main.Msg {}
simulationMain =
    Simulation.fromDocument
        { init = Main.init () (routeToUrl Route.Home) Application.dummyNavigation
        , update = Main.update Application.dummyNavigation
        , view = Main.view
        }


changeUrl : Route -> Simulation model Main.Msg ctx -> Simulation model Main.Msg ctx
changeUrl route =
    Simulation.triggerMsg (Main.urlChanged (routeToUrl route))


suite : Test
suite =
    describe "tic tac toe UI tests"
        [ test "clicking on a filled cell is a noop" <|
            \() ->
                simulationMain
                    |> changeUrl Route.TicTacToe
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
                simulationMain
                    |> changeUrl Route.TicTacToe
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
