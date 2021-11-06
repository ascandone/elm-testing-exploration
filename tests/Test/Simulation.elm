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
    )

import Browser
import Expect
import Html exposing (Html)
import Json.Encode exposing (Value)
import Test.Html.Event as Event exposing (Event)
import Test.Html.Query as Query


type alias App model msg =
    { update : msg -> model -> model
    , view : model -> Html msg
    }


type alias State_ model msg =
    { modelResult : Result String model
    , expectations : List (() -> Expect.Expectation)
    , app : App model msg
    }


type Simulation model msg ctx
    = Simulation (State_ model msg)


fromSandbox :
    { init : model
    , update : msg -> model -> model
    , view : model -> Html msg
    }
    -> Simulation model msg {}
fromSandbox flags =
    Simulation
        { modelResult = Ok flags.init
        , expectations = []
        , app =
            { update = flags.update
            , view = flags.view
            }
        }


fromDocument :
    { init : ( model, x )
    , update : msg -> model -> ( model, x )
    , view : model -> Browser.Document msg
    }
    -> Simulation model msg {}
fromDocument args =
    fromSandbox
        { init = Tuple.first args.init
        , update = \msg -> args.update msg >> Tuple.first
        , view = args.view >> .body >> Html.div []
        }


fromElement :
    { init : ( model, x )
    , update : msg -> model -> ( model, x )
    , view : model -> Html msg
    }
    -> Simulation model msg {}
fromElement args =
    fromSandbox
        { init = Tuple.first args.init
        , update = \msg -> args.update msg >> Tuple.first
        , view = args.view
        }



-- Actions


run : Simulation model msg { r | hasExpectation : () } -> Expect.Expectation
run (Simulation state) =
    case state.modelResult of
        Err err ->
            () |> Expect.all (state.expectations ++ [ \() -> Expect.fail err ])

        Ok _ ->
            Expect.all state.expectations ()


{-| Internal, DO NOT expose
-}
whenOk : (model -> State_ model msg -> State_ model msg) -> Simulation model msg ctx1 -> Simulation model msg ctx2
whenOk f (Simulation state) =
    case state.modelResult of
        Err _ ->
            Simulation state

        Ok model ->
            Simulation (f model state)


expectModel :
    (model -> Expect.Expectation)
    -> Simulation model msg r
    -> Simulation model msg { r | hasExpectation : () }
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
    -> Simulation model msg ctx
    -> Simulation model msg { r | hasExpectation : () }
expectHtml expect =
    whenOk <|
        \model state ->
            let
                expectation () =
                    state.app.view model
                        |> Query.fromHtml
                        |> expect
            in
            { state | expectations = state.expectations ++ [ expectation ] }


triggerMsg : msg -> Simulation model msg ctx -> Simulation model msg ctx
triggerMsg msg =
    whenOk <|
        \model state ->
            { state | modelResult = Ok (state.app.update msg model) }


simulateBy :
    (msg -> Result String msg)
    -> ( ( String, Value ), Query.Single msg -> Query.Single msg )
    -> Simulation model msg ctx
    -> Simulation model msg ctx
simulateBy mapper ( event, lens ) =
    whenOk <|
        \model state ->
            { state
                | modelResult =
                    state.app.view model
                        |> Query.fromHtml
                        |> lens
                        |> Event.simulate event
                        |> Event.toResult
                        |> Result.andThen mapper
                        |> Result.map (\msg -> state.app.update msg model)
            }


simulate : ( ( String, Value ), Query.Single msg -> Query.Single msg ) -> Simulation model msg ctx -> Simulation model msg ctx
simulate =
    simulateBy Ok
