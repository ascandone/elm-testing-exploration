module Page exposing (Page(..))

import Page.AsyncDemo
import Page.SelectDemo
import Page.TicTacToe


type Page
    = Home
    | TicTacToe Page.TicTacToe.Model
    | SelectDemo Page.SelectDemo.Model
    | AsyncDemo Page.AsyncDemo.Model
