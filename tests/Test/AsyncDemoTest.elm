module Test.AsyncDemoTest exposing (suite)

import Effect exposing (Effect)
import Json.Encode as Enc
import Page.AsyncDemo as Main
import Page.AsyncDemo.Data.TodoItem as TodoItem exposing (TodoItem)
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


mockOkTodoItem : TodoItem -> Result error String
mockOkTodoItem todoItem =
    Ok (Enc.encode 2 (TodoItem.encode todoItem))


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
                    |> Simulation.simulate ( Event.input "42", [ Selector.attribute Main.inputTestId ] )
                    |> Simulation.simulate ( Event.submit, [ Selector.attribute Main.searchFormId ] )
                    |> Simulation.expectHtml (Query.has [ Selector.attribute Main.loaderId ])
                    |> Simulation.run
        , test "Doesn't query empty string" <|
            \() ->
                simulation
                    |> Simulation.simulate ( Event.submit, [ Selector.attribute Main.searchFormId ] )
                    |> Simulation.expectHtml (Query.has [ Selector.attribute Main.notAskedId ])
                    |> Simulation.run
        , test "Performs query" <|
            \() ->
                simulation
                    |> Simulation.simulate ( Event.input "123", [ Selector.attribute Main.inputTestId ] )
                    |> Simulation.simulate ( Event.submit, [ Selector.attribute Main.searchFormId ] )
                    |> Simulation.handleEffect
                        (Effect.handleRequest
                            (Main.fetchTodo { id = 123, onReceived = onReceived })
                            (mockOkTodoItem
                                { userId = 0
                                , id = 123
                                , title = "example-title"
                                , completed = False
                                }
                            )
                        )
                    |> Simulation.expectHtml
                        (Query.has
                            [ Selector.attribute Main.inputTestId
                            , Selector.containing [ Selector.text "example-title" ]
                            ]
                        )
                    |> Simulation.run
        , test "Race conditions" <|
            \() ->
                simulation
                    |> Simulation.simulate ( Event.input "123", [ Selector.attribute Main.inputTestId ] )
                    |> Simulation.simulate ( Event.submit, [ Selector.attribute Main.searchFormId ] )
                    |> Simulation.simulate ( Event.input "1234", [ Selector.attribute Main.inputTestId ] )
                    |> Simulation.simulate ( Event.submit, [ Selector.attribute Main.searchFormId ] )
                    |> Simulation.handleEffect
                        (Effect.handleRequest
                            (Main.fetchTodo { id = 1234, onReceived = onReceived })
                            (mockOkTodoItem
                                { userId = 0
                                , id = 1234
                                , title = "title-1234"
                                , completed = False
                                }
                            )
                        )
                    |> Simulation.handleEffect
                        (Effect.handleRequest
                            (Main.fetchTodo { id = 123, onReceived = onReceived })
                            (mockOkTodoItem
                                { userId = 0
                                , id = 123
                                , title = "title-123"
                                , completed = False
                                }
                            )
                        )
                    |> Simulation.expectHtml
                        (Query.has
                            [ Selector.attribute Main.inputTestId
                            , Selector.containing [ Selector.text "title-1234" ]
                            ]
                        )
                    |> Simulation.run
        ]
