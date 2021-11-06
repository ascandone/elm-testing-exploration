module Main exposing
    ( Flags
    , Model
    , Msg
    , init
    , main
    )

import Application
import Browser
import Browser.Navigation as Navigation
import Html exposing (..)
import Html.Attributes as Attr
import Page exposing (Page)
import Page.Home
import Page.TicTacToe
import Route exposing (Route)
import Url exposing (Url)


type Model
    = Model
        { page : Page
        }


type Msg
    = UrlRequested Browser.UrlRequest
    | UrlChanged Url


type alias Flags =
    ()


init : Flags -> Url -> Application.Navigation msg -> ( Model, Cmd Msg )
init flags url deps =
    update deps (UrlChanged url) <|
        Model
            { page = Page.Home
            }


update : Application.Navigation Msg -> Msg -> Model -> ( Model, Cmd Msg )
update nav msg (Model model) =
    case msg of
        UrlRequested (Browser.Internal url) ->
            ( Model model
            , nav.pushUrl (Url.toString url)
            )

        UrlRequested (Browser.External href) ->
            ( Model model
            , Navigation.load href
            )

        UrlChanged url ->
            let
                ( page, cmd ) =
                    initPage (Route.fromUrl url |> Maybe.withDefault Route.Home)
            in
            ( Model { model | page = page }
            , cmd
            )


initPage : Route -> ( Page, Cmd msg )
initPage route =
    case route of
        Route.Home ->
            ( Page.Home
            , Cmd.none
            )

        Route.TicTacToe ->
            ( Page.TicTacToe ()
            , Cmd.none
            )


viewPage : Page -> Html Msg
viewPage page =
    case page of
        Page.Home ->
            Page.Home.view
                |> Html.map never

        Page.TicTacToe () ->
            Page.TicTacToe.view


viewNav : Html msg
viewNav =
    div [ Attr.class "border-b-2 shadow-xs mb-2 py-2 px-2" ]
        [ a
            [ Attr.class "underline"
            , Route.href Route.Home
            ]
            [ text " <- Home" ]
        ]


view : Model -> Browser.Document Msg
view (Model model) =
    { title = "Title"
    , body =
        [ viewNav
        , viewPage model.page
        ]
    }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


main : Program Flags ( Navigation.Key, Model ) Msg
main =
    Application.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = UrlRequested
        }
