module Page.AsyncDemo.Data.TodoItem exposing
    ( TodoItem
    , decoder
    , encode
    )

import Json.Decode as Dec
import Json.Encode as Enc exposing (Value)


type alias TodoItem =
    { userId : Int
    , id : Int
    , title : String
    , completed : Bool
    }


decoder : Dec.Decoder TodoItem
decoder =
    Dec.map4 TodoItem
        (Dec.field "userId" Dec.int)
        (Dec.field "id" Dec.int)
        (Dec.field "title" Dec.string)
        (Dec.field "completed" Dec.bool)


encode : TodoItem -> Value
encode todoItem =
    Enc.object
        [ ( "userId", Enc.int todoItem.userId )
        , ( "id", Enc.int todoItem.id )
        , ( "title", Enc.string todoItem.title )
        , ( "completed", Enc.bool todoItem.completed )
        ]
