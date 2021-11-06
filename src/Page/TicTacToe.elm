module Page.TicTacToe exposing
    ( Coords(..)
    , Model
    , Msg
    , Player(..)
    , cellClick
    , cellQuery
    , expectPlayerWon
    , init
    , update
    , view
    )

import Browser
import Expect
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events
import Json.Encode exposing (Value)
import Test.Html.Event
import Test.Html.Query as Query
import Test.Html.Selector as Selector


type GameStatus
    = Playing
    | Tie
    | Winner Player


type Coords
    = First
    | Second
    | Third


type Player
    = X
    | O


type alias Row =
    ( Maybe Player
    , Maybe Player
    , Maybe Player
    )


type alias Game =
    ( Row, Row, Row )


type Model
    = Model
        { game : Game
        , currentPlayer : Player
        }


type Msg
    = ClickedCell Coords Coords


emptyRow : Row
emptyRow =
    ( Nothing, Nothing, Nothing )


init : Model
init =
    Model
        { game = ( emptyRow, emptyRow, emptyRow )
        , currentPlayer = X
        }


currentCellValue : ( Coords, Coords ) -> Game -> Maybe Player
currentCellValue ( rowCoord, colCoord ) =
    getTriple rowCoord >> getTriple colCoord


update : Msg -> Model -> Model
update msg ((Model model) as wrapper) =
    let
        { game, currentPlayer } =
            model
    in
    case msg of
        ClickedCell rowCoord colCoord ->
            case ( currentCellValue ( rowCoord, colCoord ) game, getGameStatus game ) of
                ( Nothing, Playing ) ->
                    Model
                        { model
                            | game = game |> updateGame currentPlayer ( rowCoord, colCoord )
                            , currentPlayer = swapPlayer currentPlayer
                        }

                _ ->
                    wrapper


updateGame : Player -> ( Coords, Coords ) -> Game -> Game
updateGame player ( rowCoord, colCoord ) =
    mapTriple rowCoord
        (mapTriple colCoord (\_ -> Just player))



-- View


view : Model -> Html Msg
view (Model model) =
    div []
        [ viewGameStatus (getGameStatus model.game)
        , br [] []
        , text ("Player: " ++ playerToString model.currentPlayer)
        , hr [] []
        , viewGame model.game
        ]


viewGameStatus : GameStatus -> Html msg
viewGameStatus gameStatus =
    case gameStatus of
        Tie ->
            span [] [ text "Tie" ]

        Playing ->
            span [] [ text "" ]

        Winner player ->
            span [ playerWonTestId player ]
                [ text ("Player " ++ playerToString player ++ " won") ]


viewGame : Game -> Html Msg
viewGame ( row1, row2, row3 ) =
    div []
        [ viewRow First row1
        , viewRow Second row2
        , viewRow Third row3
        ]


viewRow : Coords -> Row -> Html Msg
viewRow coord ( a, b, c ) =
    Html.map (ClickedCell coord) <|
        div [ class "flex", rowTestId coord ]
            [ viewCell First a
            , viewCell Second b
            , viewCell Third c
            ]


viewCell : Coords -> Maybe Player -> Html Coords
viewCell coord mPlayer =
    button
        [ class "p-1 m-1 h-12 w-12 rounded hover:bg-gray-100"
        , class "flex justify-center items-center"
        , class "font-bold text-gray-900"
        , class <|
            case mPlayer of
                Nothing ->
                    "border"

                Just _ ->
                    "border-2 border-pink-300 bg-pink-50"
        , Html.Events.onClick coord
        , colTestId coord
        ]
        [ renderMaybe (text << playerToString) mPlayer
        ]



-- Utilities


renderMaybe : (a -> Html msg) -> Maybe a -> Html msg
renderMaybe f m =
    case m of
        Nothing ->
            text ""

        Just x ->
            f x


playerToString : Player -> String
playerToString player =
    case player of
        X ->
            "X"

        O ->
            "O"


mapTriple : Coords -> (a -> a) -> ( a, a, a ) -> ( a, a, a )
mapTriple coord f ( a, b, c ) =
    case coord of
        First ->
            ( f a, b, c )

        Second ->
            ( a, f b, c )

        Third ->
            ( a, b, f c )


getTriple : Coords -> ( a, a, a ) -> a
getTriple coord ( a, b, c ) =
    case coord of
        First ->
            a

        Second ->
            b

        Third ->
            c


extract : ( Maybe a, Maybe a, Maybe a ) -> Maybe a
extract triple =
    case triple of
        ( Just p1, Just p2, Just p3 ) ->
            if p1 == p2 && p2 == p3 then
                Just p1

            else
                Nothing

        _ ->
            Nothing


findWinner : List ( Maybe player, Maybe player, Maybe player ) -> Maybe player
findWinner try =
    case try of
        [] ->
            Nothing

        hd :: tl ->
            case extract hd of
                Nothing ->
                    findWinner tl

                just ->
                    just


getWinner : Game -> Maybe Player
getWinner ( ( a1, a2, a3 ), ( b1, b2, b3 ), ( c1, c2, c3 ) ) =
    findWinner
        [ ( a1, a2, a3 ) -- Horizontal
        , ( b1, b2, b3 )
        , ( c1, c2, c3 )
        , ( a1, b1, c1 ) -- Vertical
        , ( a2, b2, c2 )
        , ( a3, b3, c3 )
        , ( a1, b2, c3 ) -- Oblique
        , ( a3, b2, c1 )
        ]


isRowFull : Row -> Bool
isRowFull row =
    case row of
        ( Just _, Just _, Just _ ) ->
            True

        _ ->
            False


swapPlayer : Player -> Player
swapPlayer player =
    case player of
        X ->
            O

        O ->
            X


isTie : Game -> Bool
isTie ( r1, r2, r3 ) =
    isRowFull r1 && isRowFull r2 && isRowFull r3


getGameStatus : Game -> GameStatus
getGameStatus game =
    if isTie game then
        Tie

    else
        case getWinner game of
            Just player ->
                Winner player

            Nothing ->
                Playing



-- Main


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }



-- Testing utilities


attrTestId : String -> Attribute msg
attrTestId =
    Html.Attributes.attribute "data-test-id"


playerWonTestId : Player -> Attribute msg
playerWonTestId player =
    attrTestId ("player-" ++ playerToString player ++ "-won")


expectPlayerWon : Player -> Query.Single msg -> Expect.Expectation
expectPlayerWon player =
    Query.has
        [ Selector.attribute (playerWonTestId player)
        ]


coordToId : Coords -> String
coordToId coord =
    case coord of
        First ->
            "1"

        Second ->
            "2"

        Third ->
            "3"


rowTestId : Coords -> Attribute msg
rowTestId rowCoord =
    attrTestId ("row-" ++ coordToId rowCoord)


colTestId : Coords -> Attribute msg
colTestId colCoord =
    attrTestId ("col-" ++ coordToId colCoord)


cellQuery : Coords -> Coords -> Query.Single msg -> Query.Single msg
cellQuery rowCoord colCoord =
    Query.find
        [ Selector.attribute (rowTestId rowCoord)
        , Selector.attribute (colTestId colCoord)
        ]


cellClick : Coords -> Coords -> ( ( String, Value ), Query.Single msg -> Query.Single msg )
cellClick rowCoord colCoord =
    ( Test.Html.Event.click
    , cellQuery rowCoord colCoord
    )
