module Application exposing (Navigation, application, dummy)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Navigation
import Url exposing (Url)


type alias Keyed model =
    ( Navigation.Key, model )


type alias Navigation msg =
    { pushUrl : String -> Cmd msg
    }


navigation : Navigation.Key -> Navigation msg
navigation key =
    { pushUrl = Navigation.pushUrl key
    }


dummy : Navigation msg
dummy =
    { pushUrl = \_ -> Cmd.none
    }


application :
    { init : flags -> Url -> Navigation msg -> ( model, Cmd msg )
    , view : model -> Document msg
    , update : Navigation msg -> msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , onUrlRequest : UrlRequest -> msg
    , onUrlChange : Url.Url -> msg
    }
    -> Program flags ( Navigation.Key, model ) msg
application args =
    let
        init : flags -> Url -> Navigation.Key -> ( Keyed model, Cmd msg )
        init flags url key =
            let
                ( model, cmd ) =
                    args.init flags url (navigation key)
            in
            ( ( key, model ), cmd )

        update : msg -> Keyed model -> ( Keyed model, Cmd msg )
        update msg ( key, model ) =
            let
                ( newModel, cmd ) =
                    args.update (navigation key) msg model
            in
            ( ( key, newModel ), cmd )

        view : Keyed model -> Browser.Document msg
        view ( _, model ) =
            args.view model

        subscriptions : Keyed model -> Sub msg
        subscriptions ( _, model ) =
            args.subscriptions model
    in
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = args.onUrlChange
        , onUrlRequest = args.onUrlRequest
        }
