module Main exposing
    ( Flags
    , Model
    , Msg
    , init
    , main
    , update
    , urlChanged
    , view
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
      -- Pages
    | TicTacToeMsg Page.TicTacToe.Msg


type alias Flags =
    ()


init : Flags -> Url -> Application.Navigation Msg -> ( Model, Cmd Msg )
init () url deps =
    update deps (UrlChanged url) <|
        Model
            { page = Page.Home
            }


update : Application.Navigation Msg -> Msg -> Model -> ( Model, Cmd Msg )
update nav msg (Model model) =
    case ( msg, model.page ) of
        ( UrlRequested (Browser.Internal url), _ ) ->
            ( Model model
            , nav.pushUrl (Url.toString url)
            )

        ( UrlRequested (Browser.External href), _ ) ->
            ( Model model
            , Navigation.load href
            )

        ( UrlChanged url, _ ) ->
            let
                ( page, cmd ) =
                    initPage (Route.fromUrl url |> Maybe.withDefault Route.Home)
            in
            ( Model { model | page = page }
            , cmd
            )

        -- Pages
        ( TicTacToeMsg subMsg, Page.TicTacToe subModel ) ->
            ( Model { model | page = Page.TicTacToe (Page.TicTacToe.update subMsg subModel) }
            , Cmd.none
            )

        _ ->
            ( Model model
            , Cmd.none
            )


initPage : Route -> ( Page, Cmd msg )
initPage route =
    case route of
        Route.Home ->
            ( Page.Home
            , Cmd.none
            )

        Route.TicTacToe ->
            ( Page.TicTacToe Page.TicTacToe.init
            , Cmd.none
            )


viewPage : Page -> Html Msg
viewPage page =
    case page of
        Page.Home ->
            Page.Home.view
                |> Html.map never

        Page.TicTacToe subModel ->
            Page.TicTacToe.view subModel
                |> Html.map TicTacToeMsg


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
        , div [ Attr.class "mx-4" ]
            [ div [ Attr.class "max-w-screen-lg mx-auto" ] [ viewPage model.page ] ]
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



-- Testing utilities


urlChanged : Url -> Msg
urlChanged =
    UrlChanged
