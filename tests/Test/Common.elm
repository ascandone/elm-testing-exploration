module Test.Common exposing (changeUrl, simulationMain)

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


routeToUrl : Route -> Url
routeToUrl route =
    { protocol = Url.Https
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
