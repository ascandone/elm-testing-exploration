module Page.AsyncDemo exposing
    ( Model
    , Msg
    , fetchTodo
    , init
    , inputTestId
    , loaderId
    , notAskedId
    , searchFormId
    , update
    , view
    )

import Common exposing (dataTestId)
import Effect exposing (Effect)
import Effect.Http
import Html exposing (..)
import Html.Attributes as Attr exposing (class)
import Html.Events as Events
import Http


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
    = GotData String (Result Http.Error String)
    | Input String
    | ClickedFetch


init : ( Model, Effect Msg )
init =
    ( Model
        { remoteData = NotAsked
        , queryInput = ""
        }
    , Effect.none
    )


update : Msg -> Model -> ( Model, Effect Msg )
update msg (Model model) =
    case msg of
        Input value ->
            ( Model { model | queryInput = value }
            , Effect.none
            )

        ClickedFetch ->
            if String.isEmpty model.queryInput then
                ( Model model
                , Effect.none
                )

            else
                ( Model { model | remoteData = Loading model.queryInput }
                , fetchTodo { id = model.queryInput, onReceived = GotData model.queryInput }
                )

        GotData id data ->
            ( Model { model | remoteData = Received id data }
            , Effect.none
            )


fetchTodo : { id : String, onReceived : Result Http.Error String -> msg } -> Effect msg
fetchTodo { id, onReceived } =
    Effect.httpGet
        { url = "https://jsonplaceholder.typicode.com/todos/" ++ id
        , expect = Effect.Http.expectString onReceived
        }


view : Model -> Html Msg
view (Model model) =
    div []
        [ form [ Events.onSubmit ClickedFetch, searchFormId ]
            [ div [ class "flex" ]
                [ input
                    [ Attr.value model.queryInput
                    , Events.onInput Input
                    , Attr.placeholder "Search user..."
                    , class "border border-gray-800 px-2 py-1 rounded"
                    , inputTestId
                    ]
                    []
                , div [ class "w-2" ] []
                , button
                    [ Attr.type_ "submit"
                    , class "px-3 py-1 bg-gray-900 text-white font-medium rounded"
                    ]
                    [ text "Search" ]
                ]
            ]
        , hr [ class "my-4" ] []
        , div []
            [ case model.remoteData of
                NotAsked ->
                    span [ notAskedId ] [ text "" ]

                Loading _ ->
                    span [ loaderId ] [ text "Loading..." ]

                Received _ (Ok user) ->
                    pre [] [ text user ]

                Received _ (Err _) ->
                    text "Error."
            ]
        ]



-- Testing utilities


inputTestId : Attribute msg
inputTestId =
    dataTestId "query-input"


searchFormId : Attribute msg
searchFormId =
    dataTestId "search-form"


notAskedId : Attribute msg
notAskedId =
    dataTestId "not-asked"


loaderId : Attribute msg
loaderId =
    dataTestId "loader"
