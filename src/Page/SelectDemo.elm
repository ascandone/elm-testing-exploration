module Page.SelectDemo exposing
    ( Model
    , Msg
    , countriesSelectTestId
    , init
    , selectedCountriesNumberTestId
    , update
    , view
    )

import Common
import Components.Select as Select
import Data.Countries exposing (countries)
import Html exposing (..)
import Html.Attributes exposing (class)


type Model
    = Model
        { countriesSelect : Select.Model
        }


type Msg
    = SelectMsg Select.Msg


init : Model
init =
    Model
        { countriesSelect = Select.init countriesOption
        }


countriesOption : List Select.Option
countriesOption =
    countries
        |> List.map (\text -> { text = text, id = text })


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Model model) =
    case msg of
        SelectMsg selectMsg ->
            let
                ( updatedModel, cmd ) =
                    Select.update selectMsg model.countriesSelect
            in
            ( Model { model | countriesSelect = updatedModel }
            , Cmd.map SelectMsg cmd
            )


countriesSelectTestId : String
countriesSelectTestId =
    "countries-select"


selectedCountriesNumberTestId : Attribute msg
selectedCountriesNumberTestId =
    Common.dataTestId "selected-countries-number"


view : Model -> Html Msg
view (Model { countriesSelect }) =
    let
        selectOptions =
            Select.getSelectedOptions countriesSelect
    in
    div [ class "p-10 text-gray-800" ]
        [ div [ class "flex" ]
            [ -- Left side
              div [ class "mx-5 flex-1" ]
                [ countriesSelect
                    |> Select.view
                        { hint = "Enter Value..."
                        , testId = countriesSelectTestId
                        }
                    |> Html.map SelectMsg
                ]
            , div [ class "w-10" ] []

            -- Right side
            , div []
                [ h3 [ class "font-bold text-gray-900 text-lg" ]
                    [ text "You selected "
                    , span [ selectedCountriesNumberTestId ] [ text <| String.fromInt (List.length selectOptions) ]
                    , text " countries"
                    ]
                , ul [ class "list-disc" ]
                    (Select.getSelectedOptions countriesSelect
                        |> List.map
                            (\option ->
                                li [] [ text option.text ]
                            )
                    )
                ]
            ]
        ]
