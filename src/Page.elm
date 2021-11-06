module Page exposing (Page(..))

import Page.TicTacToe


type Page
    = Home
    | TicTacToe Page.TicTacToe.Model
