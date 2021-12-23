module Components.Select exposing
    ( Model
    , Msg
    , Option
    , deleteItem
    , focus
    , getSelectedOptions
    , init
    , input
    , selectedItemWithId
    , update
    , view
    )

import Browser.Dom as Dom
import Common
import Html exposing (..)
import Html.Attributes as A exposing (class)
import Html.Events as E
import Icons
import Json.Decode as Dec
import Json.Encode exposing (Value)
import Task
import Test.Html.Event as Event
import Test.Html.Selector as Selector exposing (Selector)


type alias Option =
    { id : String
    , text : String
    }


type Model
    = Model
        { inputText : String
        , inputIsFocused : Bool
        , indexActive : Maybe Int
        , selectedOptions : List Option
        , unselectedOptions : List Option
        , mouseDown : Bool
        }



-- Public interface


getSelectedOptions : Model -> List Option
getSelectedOptions (Model model) =
    model.selectedOptions


init : List Option -> Model
init options =
    Model
        { inputText = ""
        , inputIsFocused = False
        , indexActive = Just 0
        , selectedOptions = []
        , unselectedOptions = options
        , mouseDown = False
        }


type Msg
    = SetInput String
    | MouseDown String
    | MouseUp
    | Selected String
    | Unselected String
    | FocusedInput
    | BlurredInput
    | ClickedArea
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
        MouseDown str ->
            update (Selected str) (Model { model | mouseDown = True })

        MouseUp ->
            ( Model { model | mouseDown = False }
            , Cmd.none
            )

        FocusedInput ->
            ( Model { model | inputIsFocused = True }
            , Cmd.none
            )

        BlurredInput ->
            ( if model.mouseDown then
                Model model

              else
                Model { model | inputIsFocused = False }
            , Cmd.none
            )

        ClickedArea ->
            ( Model model
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
            case Maybe.andThen (\i -> listNth i (filteredOptions (Model model))) model.indexActive of
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
                            (\i -> min (List.length (filteredOptions (Model model)) - 1) (i + 1))
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


view : { hint : String, testId : String } -> Model -> Html Msg
view args (Model model) =
    let
        { indexActive, inputIsFocused, inputText, selectedOptions } =
            model

        viewOption : Int -> Option -> Html Msg
        viewOption filteredIndex option =
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
                    , E.onMouseDown (MouseDown option.id)
                    , E.onMouseUp MouseUp
                    , Common.dataTestId option.id
                    ]
                )
                [ dropDownItem { active = active, text = option.text } ]
    in
    div
        [ class "relative max-w-sm"
        , Common.dataTestId args.testId
        , E.onClick ClickedArea
        ]
        [ div
            [ class "border border-gray-500 flex px-1 py-1 rounded-md flex-wrap"
            , A.classList [ ( "ring", model.inputIsFocused ) ]
            ]
            (List.append
                (List.map
                    (\option ->
                        chip
                            { option = option
                            , onDelete = Unselected option.id
                            }
                    )
                    selectedOptions
                )
                [ Html.input
                    [ A.class "outline-none self-center max-w-full ml-1 h-8"
                    , A.placeholder args.hint
                    , A.value inputText
                    , E.onInput SetInput
                    , handleKeyDown
                    , A.id inputId
                    , A.autocomplete False
                    , inputTestId
                    , E.onBlur BlurredInput
                    , E.onFocus FocusedInput
                    ]
                    []
                ]
            )
        , div [ class "w-full mt-4" ]
            [ div [ class "h-52 absolute w-full" ] <|
                if inputIsFocused then
                    [ div
                        [ A.class "border h-full rounded-md w-full text-gray-800 max-h-full bg-white overflow-y-scroll"
                        , A.id dropDownId
                        ]
                        (Model model
                            |> filteredOptions
                            |> List.indexedMap viewOption
                        )
                    ]

                else
                    []
            ]
        ]


inputTestId : Attribute msg
inputTestId =
    Common.dataTestId "input"


chipTestId : String -> Attribute msg
chipTestId optionId =
    Common.dataTestId ("option-chip--" ++ optionId)


chipDeleteButtonTestId : Attribute msg
chipDeleteButtonTestId =
    Common.dataTestId "option-chip__delete"


deleteItem : String -> String -> ( ( String, Value ), List Selector )
deleteItem testId optionId =
    ( Event.click
    , [ Selector.attribute (Common.dataTestId testId)
      , Selector.attribute (chipTestId optionId)
      , Selector.attribute chipDeleteButtonTestId
      ]
    )


chip : { onDelete : msg, option : Option } -> Html msg
chip args =
    div
        [ class "rounded-lg h-8 px-3 py-1 mr-1 my-[2px] bg-gray-300 text-gray-800 flex text-sm items-center"
        , chipTestId args.option.id
        ]
        [ text args.option.text
        , i
            [ class "cursor-pointer ml-2 bg-gray-800 rounded-full self-center"
            , E.onClick args.onDelete
            , chipDeleteButtonTestId
            ]
            [ Icons.cross ]
        ]


dropDownItem : { active : Bool, text : String } -> Html msg
dropDownItem args =
    div
        [ A.class "px-2 py-1"
        , A.classList [ ( "bg-gray-200", args.active ) ]
        ]
        [ text args.text ]


stringContainsCaseInsensitive : String -> String -> Bool
stringContainsCaseInsensitive subStr str =
    String.toLower str
        |> String.contains (String.toLower subStr)


filteredOptions : Model -> List Option
filteredOptions (Model { inputText, unselectedOptions }) =
    unselectedOptions
        |> List.filter (.text >> stringContainsCaseInsensitive inputText)



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
    E.on "keydown" (Dec.map tagger E.keyCode)


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
-- Testing utilities


inputQuery : String -> List Selector
inputQuery testId =
    [ Selector.attribute (Common.dataTestId testId)
    , Selector.attribute inputTestId
    ]


focus : String -> ( ( String, Value ), List Selector )
focus testId =
    ( Event.focus
    , inputQuery testId
    )


selectedItemWithId : String -> String -> ( ( String, Value ), List Selector )
selectedItemWithId testId valueId =
    ( Event.mouseDown
    , [ Selector.attribute (Common.dataTestId testId)
      , Selector.attribute (Common.dataTestId valueId)
      ]
    )


input : String -> String -> ( ( String, Value ), List Selector )
input testId value =
    ( Event.input value
    , inputQuery testId
    )
