module Page.SelectDemo exposing
    ( Model
    , Msg
    , init
    , update
    , view
    )

import Components.Select as Select
import Data.Countries exposing (countries)
import Html as H exposing (Html)
import Html.Attributes as A


type alias Model =
    { selectModel : Select.Model
    }


selectInit : Select.Model
selectInit =
    Select.init


init : Model
init =
    { selectModel =
        { selectInit
            | unselectedOptions =
                countries
                    |> List.map (\text -> { text = text, id = text })
        }
    }


type Msg
    = SelectMsg Select.Msg


customSelectUpdate : Select.Msg -> Select.Model -> ( Select.Model, Cmd Select.Msg )
customSelectUpdate msg model =
    case msg of
        Select.BackSpace ->
            let
                ( updatedModel, cmd ) =
                    Select.update msg model
            in
            ( { updatedModel
                | inputText =
                    model.selectedOptions
                        |> List.reverse
                        |> List.head
                        |> Maybe.map .text
                        |> Maybe.withDefault model.inputText
              }
            , cmd
            )

        _ ->
            Select.update msg model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectMsg selectMsg ->
            let
                ( updatedModel, cmd ) =
                    customSelectUpdate selectMsg model.selectModel
            in
            ( { model | selectModel = updatedModel }
            , Cmd.map SelectMsg cmd
            )


view : Model -> Html Msg
view { selectModel } =
    H.div [ A.class "p-10" ]
        [ H.map SelectMsg <|
            Select.defaultView { hint = "Enter Value..." } selectModel
        , H.br [] []
        , H.text ("You selected " ++ String.fromInt (List.length selectModel.selectedOptions) ++ " countries")
        ]
