module UndoList.Extra exposing (goto, newWhenDifferent)

import List.Extra as List
import UndoList exposing (UndoList)


newWhenDifferent : state -> UndoList state -> UndoList state
newWhenDifferent newState undoList =
    if undoList.present == newState then
        undoList

    else
        UndoList.new newState undoList


doTimes : Int -> (a -> a) -> a -> a
doTimes times f x =
    if times <= 0 then
        x

    else
        doTimes (times - 1) f (f x)



{-
   goto : Int -> UndoList state -> UndoList state
   goto index undoList =
       let
           times =
               if index >= 0 then
                   UndoList.lengthPast undoList - index

               else
                   -index
       in
       doTimes times UndoList.undo undoList

-}


goto : Int -> UndoList state -> UndoList state
goto index undoList =
    UndoList.toList undoList
        |> List.splitAt index
        |> Maybe.map (\( left, middle, right ) -> UndoList (List.reverse left) middle right)
        --This should not be possible
        |> Maybe.withDefault undoList
