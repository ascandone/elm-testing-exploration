module Test.Simulation exposing
    ( Simulation
    , expectHtml
    , expectModel
    , fromDocument
    , fromElement
    , fromSandbox
    , run
    , simulate
    , simulateBy
    , triggerMsg
    , withHandler
    )

{-| Experimental testing utility

    A simple wrapper over an elm { init, update, view } record useful to perform the following steps::

    1. render the view from the current model
    2. trigger a message from a vdom event
    3. run the message over the update function and generate a new model
    4. (repeat)

-}

import Browser
import Expect
import Html exposing (Html)
import Json.Encode exposing (Value)
import Test exposing (Test)
import Test.Html.Event as Event
import Test.Html.Query as Query


type alias OkState model eff =
    { model : model
    , queue : eff
    }


type alias App model msg eff =
    { update : msg -> model -> ( model, eff )
    , view : model -> Html msg
    , batch : List eff -> eff
    }


type alias SimulationData model msg effect =
    { state : Result String (OkState model effect)
    , expectations : List (() -> Expect.Expectation)
    , app : App model msg effect
    }


type Simulation model msg effect ctx
    = Simulation (SimulationData model msg effect)


fromSandbox :
    { init : model
    , update : msg -> model -> model
    , view : model -> Html msg
    }
    -> Simulation model msg () {}
fromSandbox flags =
    Simulation
        { state =
            Ok
                { model = flags.init
                , queue = ()
                }
        , expectations = []
        , app =
            { update = \msg model -> ( flags.update msg model, () )
            , view = flags.view
            , batch = always ()
            }
        }


fromElement :
    { init : ( model, eff )
    , update : msg -> model -> ( model, eff )
    , view : model -> Html msg
    , batch : List eff -> eff
    }
    -> Simulation model msg eff {}
fromElement flags =
    let
        ( model, effect ) =
            flags.init
    in
    Simulation
        { state =
            Ok
                { model = model
                , queue = effect
                }
        , expectations = []
        , app =
            { update = flags.update
            , view = flags.view
            , batch = flags.batch
            }
        }


fromDocument :
    { init : ( model, effect )
    , update : msg -> model -> ( model, effect )
    , view : model -> Browser.Document msg
    , batch : List effect -> effect
    }
    -> Simulation model msg effect {}
fromDocument flags =
    fromElement
        { init = flags.init
        , update = flags.update
        , view = flags.view >> .body >> Html.div []
        , batch = flags.batch
        }



-- Actions


run : Simulation model msg eff { r | hasExpectation : () } -> Expect.Expectation
run (Simulation state) =
    case state.state of
        Err err ->
            () |> Expect.all (state.expectations ++ [ \() -> Expect.fail err ])

        Ok _ ->
            Expect.all state.expectations ()


{-| Internal, DO NOT expose
-}
whenOk : (OkState model eff -> SimulationData model msg eff -> SimulationData model msg eff) -> Simulation model msg eff ctx1 -> Simulation model msg eff ctx2
whenOk f (Simulation simulation) =
    Simulation <|
        case simulation.state of
            Err _ ->
                simulation

            Ok okState ->
                f okState simulation


expectModel : (OkState model eff -> Expect.Expectation) -> Simulation model msg eff ctx -> Simulation model msg eff { ctx | hasExpectation : () }
expectModel expect =
    whenOk <|
        \model state ->
            let
                expectation () =
                    expect model
            in
            { state | expectations = state.expectations ++ [ expectation ] }


expectHtml :
    (Query.Single msg -> Expect.Expectation)
    -> Simulation model msg eff ctx
    -> Simulation model msg eff { ctx | hasExpectation : () }
expectHtml expect =
    whenOk <|
        \{ model } state ->
            let
                expectation () =
                    state.app.view model
                        |> Query.fromHtml
                        |> expect
            in
            { state | expectations = state.expectations ++ [ expectation ] }


triggerMsg : msg -> Simulation model msg eff ctx -> Simulation model msg eff ctx
triggerMsg msg =
    whenOk <|
        \{ model, queue } state ->
            let
                ( newModel, effect ) =
                    state.app.update msg model
            in
            { state
                | state =
                    Ok
                        { model = newModel
                        , queue = state.app.batch [ queue, effect ]
                        }
            }


withHandler : (effect -> Maybe ( msg, effect )) -> Simulation model msg effect ctx -> Simulation model msg effect ctx
withHandler handler ((Simulation simulation) as wrapper) =
    case simulation.state of
        Err _ ->
            wrapper

        Ok okState ->
            case handler okState.queue of
                Nothing ->
                    -- TODO better error message
                    Simulation { simulation | state = Err "handler error" }

                Just ( msg, newQueue ) ->
                    -- TODO update queue
                    triggerMsg msg wrapper


getMsgResult : ( ( String, Value ), Query.Single msg -> Query.Single msg ) -> OkState model effect -> Simulation model msg effect ctx -> Result String msg
getMsgResult ( event, lens ) okState (Simulation simulation) =
    simulation.app.view okState.model
        |> Query.fromHtml
        |> lens
        |> Event.simulate event
        |> Event.toResult


simulateBy :
    (msg -> Result String msg)
    -> ( ( String, Value ), Query.Single msg -> Query.Single msg )
    -> Simulation model msg eff ctx
    -> Simulation model msg eff ctx
simulateBy extractMsg pair ((Simulation simulation) as wrapper) =
    case simulation.state of
        Err _ ->
            wrapper

        Ok okState ->
            case
                getMsgResult pair okState wrapper
                    |> Result.andThen
                        (\msg -> extractMsg msg |> Result.map (\newMsg -> ( msg, newMsg )))
            of
                Err e ->
                    Simulation { simulation | state = Err e }

                Ok ( msg, newMsg ) ->
                    wrapper
                        |> triggerMsg msg
                        |> triggerMsg newMsg


simulate : ( ( String, Value ), Query.Single msg -> Query.Single msg ) -> Simulation model msg eff ctx -> Simulation model msg eff ctx
simulate pair ((Simulation simulation) as wrapper) =
    case simulation.state of
        Err _ ->
            wrapper

        Ok okState ->
            case getMsgResult pair okState wrapper of
                Err e ->
                    Simulation { simulation | state = Err e }

                Ok msg ->
                    triggerMsg msg wrapper
