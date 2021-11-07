module Components.Select exposing
    ( Model
    , Msg
    , Option
    , ViewArgs
    , chip
    , defaultView
    , dropDownItem
    , getSelectedOptions
    , init
    , update
    , view
    )

import Browser.Dom as Dom
import Html exposing (..)
import Html.Attributes as A exposing (class)
import Html.Events as E
import Json.Decode as Json
import Svg
import Svg.Attributes
import Svg.Events
import Task


type alias Option =
    { id : String
    , text : String
    }


type Model
    = Model
        { inputText : String
        , showDropdown : Bool
        , indexActive : Maybe Int
        , selectedOptions : List Option
        , unselectedOptions : List Option
        }



-- Public interface


getSelectedOptions : Model -> List Option
getSelectedOptions (Model model) =
    model.selectedOptions


init : List Option -> Model
init options =
    Model
        { inputText = ""
        , showDropdown = False
        , indexActive = Just 0
        , selectedOptions = []
        , unselectedOptions = options
        }


type Msg
    = SetInput String
    | Selected String
    | Unselected String
    | ShowDropdown Bool
    | SetActive (Maybe Int)
    | ArrowDown
    | ArrowUp
    | BackSpace
    | Enter
    | Esc
    | KeyDown Int
    | Noop


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Model model) =
    case msg of
        ShowDropdown b ->
            ( Model { model | showDropdown = b }
            , Cmd.none
            )

        SetInput str ->
            ( Model
                { model
                    | inputText = str
                    , indexActive = Just 0
                }
            , Cmd.none
            )

        SetActive x ->
            ( Model
                { model
                    | indexActive = x
                }
            , Cmd.none
            )

        Esc ->
            ( Model model
            , blurInput
            )

        Enter ->
            case Maybe.andThen (\i -> listNth i (filteredItems (Model model))) model.indexActive of
                Nothing ->
                    ( Model model
                    , Cmd.none
                    )

                Just item ->
                    ( Model
                        { model
                            | indexActive = Just 0
                            , selectedOptions = model.selectedOptions ++ [ item ]
                            , unselectedOptions = List.filter (.id >> (/=) item.id) model.unselectedOptions
                            , inputText = ""
                        }
                    , Cmd.batch
                        [ Task.attempt (always Noop) (Dom.setViewportOf dropDownId 0 0)
                        , focusInput
                        ]
                    )

        ArrowDown ->
            ( Model
                { model
                    | indexActive =
                        Maybe.map
                            (\i -> min (List.length (filteredItems (Model model)) - 1) (i + 1))
                            model.indexActive
                }
            , Task.map3
                (\item cont cont_ ->
                    ( { height = item.element.height
                      , y = item.element.y - cont_.element.y --dynamic
                      }
                    , { totalHeight = cont.scene.height
                      , visibleHeight = cont.viewport.height
                      , scrollTop = cont.viewport.y -- dynamic
                      }
                    )
                )
                (Dom.getElement activeItemId)
                (Dom.getViewportOf dropDownId)
                (Dom.getElement dropDownId)
                |> Task.andThen
                    (\( item, cont ) ->
                        let
                            scrollAmt =
                                item.height + item.y - cont.visibleHeight
                        in
                        if scrollAmt > 0 then
                            Dom.setViewportOf dropDownId 0 (cont.scrollTop + scrollAmt - 2)

                        else
                            Task.succeed ()
                    )
                |> Task.attempt (always Noop)
            )

        ArrowUp ->
            ( Model
                { model
                    | indexActive =
                        Maybe.map
                            (\i -> max 0 (i - 1))
                            model.indexActive
                }
            , Task.map3
                --TODO: refactor
                (\item cont cont_ ->
                    ( { height = item.element.height
                      , y = item.element.y - cont_.element.y - 2 --dynamic
                      }
                    , { totalHeight = cont.scene.height
                      , visibleHeight = cont.viewport.height
                      , scrollTop = cont.viewport.y -- dynamic
                      }
                    )
                )
                (Dom.getElement activeItemId)
                (Dom.getViewportOf dropDownId)
                (Dom.getElement dropDownId)
                |> Task.andThen
                    (\( item, cont ) ->
                        if item.y < 0 then
                            Dom.setViewportOf dropDownId 0 (cont.scrollTop + item.y + 2)

                        else
                            Task.succeed ()
                    )
                |> Task.attempt (always Noop)
            )

        BackSpace ->
            if model.inputText /= "" || List.isEmpty model.selectedOptions then
                ( Model model
                , Cmd.none
                )

            else
                let
                    splitIndex =
                        List.length model.selectedOptions - 1
                in
                ( Model
                    { model
                        | selectedOptions = List.take splitIndex model.selectedOptions
                        , unselectedOptions = model.unselectedOptions ++ List.drop splitIndex model.selectedOptions
                    }
                , focusInput
                )

        Selected id ->
            let
                ( eqId, notEqId ) =
                    List.partition (.id >> (==) id) model.unselectedOptions
            in
            ( Model
                { model
                    | selectedOptions = model.selectedOptions ++ eqId
                    , unselectedOptions = notEqId
                    , inputText = ""
                }
            , focusInput
            )

        Unselected i ->
            let
                ( eqId, notEqId ) =
                    List.partition (.id >> (==) i) model.selectedOptions
            in
            ( Model
                { model
                    | selectedOptions = notEqId
                    , unselectedOptions = model.unselectedOptions ++ eqId -- TODO: sort
                }
            , Cmd.none
            )

        KeyDown _ ->
            ( Model model
            , Cmd.none
            )

        Noop ->
            ( Model model
            , Cmd.none
            )



-- View


view : ViewArgs Msg -> Model -> Html Msg
view args (Model model) =
    let
        { hint, viewUnselected, viewSelected } =
            args

        { indexActive, showDropdown, inputText, selectedOptions } =
            model

        dropDownEntry : Int -> Option -> Html Msg
        dropDownEntry filteredIndex option =
            let
                active =
                    indexActive
                        |> Maybe.map ((==) filteredIndex)
                        |> Maybe.withDefault False

                withId attrs =
                    if active then
                        A.id activeItemId :: attrs

                    else
                        attrs
            in
            div
                (withId
                    [ A.class "cursor-pointer"
                    , E.onMouseOver (SetActive <| Just filteredIndex)
                    , E.onMouseDown (Selected option.id)
                    ]
                )
                [ viewUnselected { active = active, text = option.text } ]
    in
    cls "relative max-w-sm"
        [ div
            [ class "border border-gray-500 flex px-1 py-1 rounded-md flex-wrap"
            , A.classList [ ( "ring", model.showDropdown ) ]
            ]
            (List.append
                (List.map (\{ text, id } -> viewSelected { text = text, onDelete = Unselected id }) selectedOptions)
                [ input
                    [ A.class "outline-none self-center max-w-full ml-1 h-8"
                    , A.placeholder hint
                    , A.value inputText
                    , E.onFocus (ShowDropdown True)
                    , E.onBlur (ShowDropdown False)
                    , E.onInput SetInput
                    , handleKeyDown
                    , A.id inputId
                    , A.autocomplete False
                    ]
                    []
                ]
            )
        , div [ class "w-full mt-4" ]
            [ div
                [ class "h-52 absolute w-full"
                , A.classList [ ( "hidden", not showDropdown ) ]
                ]
                [ div
                    [ A.class "border h-full rounded-md w-full text-gray-800 max-h-full bg-white overflow-y-scroll"
                    , A.id dropDownId
                    ]
                    (List.indexedMap dropDownEntry (filteredItems (Model model)))
                ]
            ]
        ]


chip : { onDelete : msg, text : String } -> Html msg
chip args =
    let
        icon =
            Svg.svg
                [ Svg.Attributes.class "h-4 w-4 cursor-pointer ml-2 bg-gray-800 rounded-full self-center"
                , Svg.Events.onClick args.onDelete
                , Svg.Attributes.style "fill: white"
                , Svg.Attributes.height "24"
                , Svg.Attributes.viewBox "0 0 24 24"
                , Svg.Attributes.width "24"
                ]
                [ Svg.path
                    [ Svg.Attributes.d "M19 6.41L17.59 5 12 10.59 6.41 5 5 6.41 10.59 12 5 17.59 6.41 19 12 13.41 17.59 19 19 17.59 13.41 12z" ]
                    []
                , Svg.path [ Svg.Attributes.d "M0 0h24v24H0z", Svg.Attributes.fill "none" ] []
                ]
    in
    cls "rounded-full h-8 px-3 py-1 mr-1 my-px bg-gray-300 text-gray-800 flex text-sm items-center"
        [ text args.text, icon ]


dropDownItem : { active : Bool, text : String } -> Html msg
dropDownItem args =
    div
        [ A.class "px-2 py-1"
        , A.classList [ ( "bg-gray-200", args.active ) ]
        ]
        [ text args.text ]


type alias ViewArgs msg =
    { hint : String
    , viewSelected :
        { onDelete : msg
        , text : String
        }
        -> Html msg
    , viewUnselected :
        { active : Bool
        , text : String
        }
        -> Html msg
    }


filteredItems : Model -> List Option
filteredItems (Model { inputText, unselectedOptions }) =
    unselectedOptions
        |> List.filter (.text >> String.toUpper >> String.contains (String.toUpper inputText))


defaultView : { hint : String } -> Model -> Html Msg
defaultView { hint } =
    view
        { hint = hint
        , viewSelected = chip
        , viewUnselected = dropDownItem
        }



-- Utilities


idNameSpace : String
idNameSpace =
    "@@elm-select__"


inputId : String
inputId =
    idNameSpace ++ "input"


activeItemId : String
activeItemId =
    idNameSpace ++ "active"


dropDownId : String
dropDownId =
    idNameSpace ++ "dropDown"



{-
   37 Left
   39 Right
-}


tagger : Int -> Msg
tagger key =
    case key of
        38 ->
            ArrowUp

        40 ->
            ArrowDown

        8 ->
            BackSpace

        13 ->
            Enter

        27 ->
            Esc

        _ ->
            KeyDown key


handleKeyDown : Attribute Msg
handleKeyDown =
    E.on "keydown" (Json.map tagger E.keyCode)


cls : String -> List (Html msg) -> Html msg
cls str children =
    div [ A.class str ] children


focusInput : Cmd Msg
focusInput =
    Task.attempt (\_ -> Noop) (Dom.focus inputId)


blurInput : Cmd Msg
blurInput =
    Task.attempt (\_ -> Noop) (Dom.blur inputId)


listNth : Int -> List a -> Maybe a
listNth i =
    List.drop i >> List.head



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
