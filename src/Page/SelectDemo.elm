module Page.SelectDemo exposing
    ( Model
    , Msg
    , init
    , update
    , view
    )

import Components.Select as Select
import Data.Countries exposing (countries)
import Html exposing (..)
import Html.Attributes exposing (class)


type alias Model =
    { selectModel : Select.Model
    }


init : Model
init =
    { selectModel =
        Select.init
            (countries
                |> List.map (\text -> { text = text, id = text })
            )
    }


type Msg
    = SelectMsg Select.InternalMsg



{-

   customSelectUpdate : Select.InternalMsg -> Select.Model -> ( Select.Model, Cmd Select.InternalMsg )
   customSelectUpdate msg model =
       case msg of
           Select.BackSpace ->
               let
                   ( updatedModel, cmd, _ ) =
                       Select.update msg model
               in
               ( { updatedModel
                   | inputText =
                       model
                           |> Select.getSelectedOptions
                           |> List.reverse
                           |> List.head
                           |> Maybe.map .text
                           |> Maybe.withDefault model.inputText
                 }
               , cmd
               )

           _ ->
               Select.update msg model

-}


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectMsg selectMsg ->
            let
                ( updatedModel, cmd, _ ) =
                    Select.update selectMsg model.selectModel
            in
            ( { model | selectModel = updatedModel }
            , Cmd.map SelectMsg cmd
            )


view : Model -> Html Msg
view { selectModel } =
    div [ class "p-10 text-gray-800" ]
        [ div [ class "flex" ]
            [ -- Left side
              div [ class "mx-5 flex-1" ]
                [ Html.map SelectMsg <|
                    Select.defaultView { hint = "Enter Value..." } selectModel
                ]
            , div [ class "w-10" ] []

            -- Right side
            , div []
                [ h3 [ class "font-bold text-gray-900 text-lg" ]
                    [ text ("You selected " ++ String.fromInt (List.length (Select.getSelectedOptions selectModel)) ++ " countries")
                    ]
                , ul [ class "list-disc" ]
                    (Select.getSelectedOptions selectModel
                        |> List.map
                            (\option ->
                                li [] [ text option.text ]
                            )
                    )
                ]
            ]
        ]
