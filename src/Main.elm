module Main exposing
    ( Flags
    , Model
    , Msg
    , init
    , main
    )

import Browser
import Browser.Navigation as Navigation
import Html exposing (..)
import Url exposing (Url)


type Model
    = Model {}


type Msg
    = UrlRequested Browser.UrlRequest
    | UrlChanged Url


type alias Flags =
    ()


init : Flags -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init _ _ _ =
    ( Model {}
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        _ ->
            ( model
            , Cmd.none
            )


view : Model -> Browser.Document Msg
view _ =
    { title = "Title"
    , body = [ text "Hello world" ]
    }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = UrlRequested
        }
