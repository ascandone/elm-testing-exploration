module Page.AsyncDemo exposing
    ( Model
    , Msg
    , init
    , update
    , view
    )

import Html exposing (..)
import Html.Attributes as Attr exposing (class)
import Html.Events as Events
import Http
import Json.Encode as Enc exposing (Value)
import Process
import Task


type RemoteData id value
    = NotAsked
    | Loading id
    | Received id (Result Http.Error value)


type Model
    = Model
        { remoteData : RemoteData String String
        , queryInput : String
        }


type Msg
    = GotData
        { url : String
        , data : Result Http.Error String
        }
    | Input String
    | ClickedFetch


init : ( Model, Cmd Msg )
init =
    ( Model
        { remoteData = NotAsked
        , queryInput = ""
        }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Model model) =
    case msg of
        Input value ->
            ( Model { model | queryInput = value }
            , Cmd.none
            )

        ClickedFetch ->
            ( Model { model | remoteData = Loading model.queryInput }
            , mockFetch model.queryInput
            )

        GotData { url, data } ->
            ( Model { model | remoteData = Received url data }
            , Cmd.none
            )


view : Model -> Html Msg
view (Model model) =
    div []
        [ form [ Events.onSubmit ClickedFetch ]
            [ input
                [ Attr.value model.queryInput
                , Events.onInput Input
                , Attr.placeholder "Search user..."
                , class "border border-black"
                ]
                []
            , div [ class "mt-2" ] []
            , button
                [ Attr.type_ "submit"
                , class "px-3 py-1 bg-gray-900 text-white font-medium rounded"
                ]
                [ text "Search" ]
            ]
        , hr [ class "my-4" ] []
        , div []
            [ case model.remoteData of
                NotAsked ->
                    text ""

                Loading _ ->
                    text "Loading..."

                Received _ (Ok user) ->
                    pre [] [ text user ]

                Received _ (Err _) ->
                    text "Error."
            ]
        ]


mockFetch : String -> Cmd Msg
mockFetch user =
    Process.sleep 1000
        |> Task.perform
            (\() ->
                GotData
                    { url = "/api/users/" ++ user
                    , data =
                        Enc.object
                            [ ( "name", Enc.string user )
                            , ( "age", Enc.int 42 )
                            ]
                            |> Enc.encode 2
                            |> Ok
                    }
            )
