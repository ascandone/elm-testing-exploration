module Effect exposing
    ( Effect
    , batch
    , handleGet
    , handleRequest
    , httpGet
    , isNone
    , map
    , none
    , run
    , suite
    )

import Effect.Http
import Expect
import Http
import Test exposing (Test)


type Effect msg
    = Cmd (Cmd msg) -- Escape hatch
    | Batch (List (Effect msg))
    | HttpRequest (Effect.Http.Request msg)


{-| Makes an handler recursive, handles Batch navigation
-}
makeHandler : (Effect msg -> Maybe msg) -> Effect msg -> Maybe ( msg, Effect msg )
makeHandler handler =
    let
        enhancedHandler effect =
            case handler effect of
                Just msg ->
                    Just ( msg, none )

                Nothing ->
                    Nothing

        go effect =
            case effect of
                Batch [] ->
                    Nothing

                Batch (recEffect :: effects) ->
                    case go recEffect of
                        Just pair ->
                            Just pair

                        Nothing ->
                            go (Batch effects)

                _ ->
                    enhancedHandler effect
    in
    go


isNone : Effect x -> Bool
isNone eff =
    case eff of
        Batch [] ->
            True

        Batch effs ->
            List.all isNone effs

        _ ->
            False


none : Effect msg
none =
    Batch []


batch : List (Effect msg) -> Effect msg
batch effs =
    {-
       Same as `batch = Batch`
       Just an optimization
    -}
    case effs of
        [ Batch [], eff ] ->
            eff

        [ eff, Batch [] ] ->
            eff

        _ ->
            Batch effs



-- Http


httpGet :
    { url : String
    , expect : Effect.Http.Expect msg
    }
    -> Effect msg
httpGet =
    HttpRequest << Effect.Http.get


handleGet : String -> Result Http.Error String -> Effect msg -> Maybe ( msg, Effect msg )
handleGet str response =
    makeHandler <|
        \effect ->
            case effect of
                HttpRequest request ->
                    Effect.Http.handleGet str response request

                _ ->
                    Nothing


handleRequest : Effect x -> Result Http.Error String -> Effect msg -> Maybe ( msg, Effect msg )
handleRequest guard response =
    makeHandler <|
        \effect ->
            case ( guard, effect ) of
                ( HttpRequest guardRequest, HttpRequest request ) ->
                    Effect.Http.handleRequest guardRequest response request

                _ ->
                    Nothing



-- Functor instance


map : (msg -> a) -> Effect msg -> Effect a
map f effect =
    case effect of
        Cmd cmd ->
            Cmd (Cmd.map f cmd)

        Batch effects ->
            Batch (List.map (map f) effects)

        HttpRequest req ->
            HttpRequest (Effect.Http.map f req)



-- Perform effect


run : Effect msg -> Cmd msg
run effect =
    case effect of
        Cmd cmd ->
            cmd

        Batch effects ->
            Cmd.batch (List.map run effects)

        HttpRequest req ->
            Effect.Http.run req



-- Specs


suite : Test
suite =
    Test.describe "Effect internals"
        [ Test.describe "IsNone" <|
            [ Test.test "Http.get" <|
                \() ->
                    httpGet { url = "/api/users", expect = Effect.Http.expectWhatever (\_ -> ()) }
                        |> isNone
                        |> Expect.false "Http.get is not None"
            , Test.test "Simple" <|
                \() ->
                    Batch []
                        |> isNone
                        |> Expect.true "should be true"
            , Test.test "Nested" <|
                \() ->
                    Batch [ Batch [], Batch [ Batch [] ] ]
                        |> isNone
                        |> Expect.true "should be true"
            , Test.test "Nested with cmd" <|
                \() ->
                    Batch [ Batch [ Cmd Cmd.none ], Batch [ Batch [] ] ]
                        |> isNone
                        |> Expect.false "should be false"
            ]
        , Test.describe "Handlers" <|
            let
                get_ url =
                    httpGet { url = url, expect = Effect.Http.expectString identity }

                expectation eff =
                    eff
                        |> handleRequest (get_ "/api/users") (Ok "data")
                        |> (\maybeMsg ->
                                case maybeMsg of
                                    Just ( msg, newEffect ) ->
                                        Expect.all
                                            [ \() -> msg |> Expect.equal (Ok "data")
                                            , \() -> isNone newEffect |> Expect.true "Should be true"
                                            ]
                                            ()

                                    Nothing ->
                                        Expect.fail "Should return the effect"
                           )
            in
            [ Test.test "Simple effect" <|
                \() ->
                    get_ "/api/users"
                        |> expectation
            , Test.test "Nested effects" <|
                \() ->
                    Batch [ Batch [], Batch [ get_ "/api/users" ] ]
                        |> expectation
            ]
        ]
