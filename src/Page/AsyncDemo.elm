module Page.AsyncDemo exposing
    ( Model
    , Msg
    , init
    , update
    , view
    )

import Html exposing (..)


type Model
    = Model {}


type Msg
    = Msg


init : ( Model, Cmd Msg )
init =
    ( Model {}
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Model model) =
    case msg of
        _ ->
            ( Model model
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    text "TODO async"
