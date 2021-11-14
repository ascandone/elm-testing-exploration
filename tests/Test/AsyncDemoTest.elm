module Test.AsyncDemoTest exposing (suite)

import Effect exposing (Effect)
import Page.AsyncDemo as Main
import Test exposing (..)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import Test.Simulation as Simulation exposing (Simulation)


simulation : Simulation Main.Model Main.Msg (Effect Main.Msg) {}
simulation =
    Simulation.fromElement
        { init = Main.init
        , update = Main.update
        , view = Main.view
        , batch = Effect.batch
        }


onReceived : x -> ()
onReceived _ =
    ()


suite : Test
suite =
    describe "AsyncDemo tests"
        [ test "Shows notasked screen" <|
            \() ->
                simulation
                    |> Simulation.expectHtml (Query.has [ Selector.attribute Main.notAskedId ])
                    |> Simulation.run
        , test "Shows loading screen" <|
            \() ->
                simulation
                    |> Simulation.simulate ( Event.input "42", Query.find [ Selector.attribute Main.inputTestId ] )
                    |> Simulation.simulate ( Event.submit, Query.find [ Selector.attribute Main.searchFormId ] )
                    |> Simulation.expectHtml (Query.has [ Selector.attribute Main.loaderId ])
                    |> Simulation.run
        , test "Doesn't query empty string" <|
            \() ->
                simulation
                    |> Simulation.simulate ( Event.submit, Query.find [ Selector.attribute Main.searchFormId ] )
                    |> Simulation.expectHtml (Query.has [ Selector.attribute Main.notAskedId ])
                    |> Simulation.run
        , test "Performs query" <|
            \() ->
                simulation
                    |> Simulation.simulate ( Event.input "abc", Query.find [ Selector.attribute Main.inputTestId ] )
                    |> Simulation.simulate ( Event.submit, Query.find [ Selector.attribute Main.searchFormId ] )
                    |> Simulation.withHandler
                        (Effect.handleRequest
                            (Main.fetchTodo { id = "abc", onReceived = onReceived })
                            (Ok "data")
                        )
                    |> Simulation.expectHtml (Query.hasNot [ Selector.attribute Main.loaderId ])
                    |> Simulation.run
        , test "Race conditions" <|
            \() ->
                simulation
                    |> Simulation.simulate ( Event.input "abc", Query.find [ Selector.attribute Main.inputTestId ] )
                    |> Simulation.simulate ( Event.submit, Query.find [ Selector.attribute Main.searchFormId ] )
                    |> Simulation.simulate ( Event.input "abcde", Query.find [ Selector.attribute Main.inputTestId ] )
                    |> Simulation.simulate ( Event.submit, Query.find [ Selector.attribute Main.searchFormId ] )
                    |> Simulation.withHandler
                        (Effect.handleRequest
                            (Main.fetchTodo { id = "abcde", onReceived = onReceived })
                            (Ok "data")
                        )
                    |> Simulation.withHandler
                        (Effect.handleRequest
                            (Main.fetchTodo { id = "abc", onReceived = onReceived })
                            (Ok "data")
                        )
                    |> Simulation.expectHtml (Query.hasNot [ Selector.attribute Main.loaderId ])
                    |> Simulation.run
        ]
