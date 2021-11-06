module Route exposing
    ( Route(..)
    , fromUrl
    , href
    , toString
    )

import Html exposing (Attribute)
import Html.Attributes
import Url exposing (Url)
import Url.Builder as Builder
import Url.Parser as Parser exposing (Parser, s, top)


type Route
    = Home
    | TicTacToe


parser : Parser (Route -> c) c
parser =
    Parser.oneOf
        [ Parser.map Home top
        , Parser.map TicTacToe <| s "tictactoe"
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


href : Route -> Attribute msg
href =
    toString >> Html.Attributes.href
