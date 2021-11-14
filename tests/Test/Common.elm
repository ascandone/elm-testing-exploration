module Test.Common exposing (changeUrl, simulationMain)

import Application
import Main
import Page.TicTacToe exposing (Coords(..), Player(..))
import Route exposing (Route)
import Test exposing (..)
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


simulationMain : Simulation Main.Model Main.Msg (Cmd Main.Msg) {}
simulationMain =
    Simulation.fromDocument
        { init = Main.init () (routeToUrl Route.Home) Application.dummyNavigation
        , update = Main.update Application.dummyNavigation
        , view = Main.view
        , batch = Cmd.batch
        }


changeUrl : Route -> Simulation model Main.Msg (Cmd Main.Msg) ctx -> Simulation model Main.Msg (Cmd Main.Msg) ctx
changeUrl route =
    Simulation.triggerMsg (Main.urlChanged (routeToUrl route))
