module List.Extra exposing (splitAt, takeDrop)


takeDrop : Int -> List a -> ( List a, List a )
takeDrop index list =
    if index <= 0 then
        ( [], list )

    else
        case list of
            [] ->
                ( [], [] )

            hd :: tl ->
                let
                    ( left, right ) =
                        takeDrop (index - 1) tl
                in
                ( hd :: left
                , right
                )


splitAt : Int -> List a -> Maybe ( List a, a, List a )
splitAt index list =
    case list of
        [] ->
            Nothing

        [ x ] ->
            Just ( [], x, [] )

        hd :: tl ->
            if index <= 0 then
                Just ( [], hd, tl )

            else
                splitAt (index - 1) tl
                    |> Maybe.map (\( left, middle, right ) -> ( hd :: left, middle, right ))
