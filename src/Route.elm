module Route exposing
    ( Route(..)
    , fromUrl
    , href
    , toString
    )

import Html exposing (Attribute)
import Html.Attributes
import Url exposing (Url)
import Url.Parser as Parser exposing (Parser, s, top)


type Route
    = Home
    | TicTacToe
    | SelectDemo
    | AsyncDemo


parser : Parser (Route -> c) c
parser =
    Parser.oneOf
        [ Parser.map Home top
        , Parser.map TicTacToe <| s "tictactoe"
        , Parser.map SelectDemo <| s "select-demo"
        , Parser.map AsyncDemo <| s "async-demo"
        ]


fromUrl : Url -> Maybe Route
fromUrl =
    Parser.parse parser


toString : Route -> String
toString route =
    case route of
        Home ->
            "/"

        TicTacToe ->
            "/tictactoe"

        SelectDemo ->
            "/select-demo"

        AsyncDemo ->
            "/async-demo"


href : Route -> Attribute msg
href =
    toString >> Html.Attributes.href
