module Page.AsyncDemo.Data.TodoItem exposing
    ( TodoItem
    , decoder
    )

import Json.Decode as Dec


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
