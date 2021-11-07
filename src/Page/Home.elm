module Page.Home exposing (view)

import Html exposing (..)
import Html.Attributes exposing (class)
import Route


viewLink route label =
    li []
        [ a [ Route.href route, class "underline text-blue-900" ]
            [ text label ]
        ]


view : Html Never
view =
    div [ class "px-10 py-1" ]
        [ h1 [ class "font-bold text-xl" ] [ text "Links:" ]
        , ul [ class "list-disc" ]
            [ viewLink Route.TicTacToe "Tic tac toe"
            , viewLink Route.SelectDemo "Multiselect component"
            , viewLink Route.AsyncDemo "Async"
            ]
        ]
