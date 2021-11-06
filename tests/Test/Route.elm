module Test.Route exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Route exposing (Route)
import Test exposing (..)
import Url


suite : Test
suite =
    describe "Routing"
        [ Test.fuzz fuzzRoute "Route parsing" <|
            \route ->
                route
                    |> Route.toString
                    |> Url.fromString
                    |> Maybe.andThen Route.fromUrl
                    |> Expect.equal (Just route)
        ]


fuzzRoute : Fuzzer Route
fuzzRoute =
    Fuzz.oneOf
        [ Fuzz.constant Route.Home
        , Fuzz.constant Route.TicTacToe
        ]
