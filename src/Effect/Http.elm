module Effect.Http exposing
    ( Body
    , Expect
    , Header
    , Request
    , emptyBody
    , expectJson
    , expectString
    , expectWhatever
    , get
    , handleGet
    , handleRequest
    , header
    , jsonBody
    , map
    , request
    , run
    , stringBody
    )

import Http
import Json.Decode as Dec exposing (Decoder)
import Json.Encode exposing (Value)



-- Create


type Request msg
    = Request
        { method : String
        , headers : List Header
        , url : String
        , body : Body
        , expect : Expect msg
        , timeout : Maybe Float
        , tracker : Maybe String
        }


request :
    { method : String
    , headers : List Header
    , url : String
    , body : Body
    , expect : Expect msg
    , timeout : Maybe Float
    , tracker : Maybe String
    }
    -> Request msg
request =
    Request


map : (msg -> a) -> Request msg -> Request a
map f (Request req) =
    let
        (Expect toMsg) =
            req.expect
    in
    Request
        { method = req.method
        , headers = req.headers
        , url = req.url
        , body = req.body
        , expect = Expect (f << toMsg)
        , timeout = req.timeout
        , tracker = req.tracker
        }


get :
    { url : String
    , expect : Expect msg
    }
    -> Request msg
get r =
    request
        { method = "GET"
        , headers = []
        , url = r.url
        , body = emptyBody
        , expect = r.expect
        , timeout = Nothing
        , tracker = Nothing
        }



-- Expect


type Expect msg
    = Expect (Result Http.Error String -> msg)


expectString : (Result Http.Error String -> msg) -> Expect msg
expectString =
    Expect


makeExpect : Expect msg -> Http.Expect msg
makeExpect (Expect toMsg) =
    Http.expectString toMsg


expectWhatever : (Result Http.Error () -> msg) -> Expect msg
expectWhatever toMsg =
    expectString (Result.map (\_ -> ()) >> toMsg)


decodeResult : Decoder a -> String -> Result Http.Error a
decodeResult decoder result =
    result
        |> Dec.decodeString decoder
        |> Result.mapError Dec.errorToString
        |> Result.mapError Http.BadBody


expectJson : (Result Http.Error a -> msg) -> Decoder a -> Expect msg
expectJson toMsg decoder =
    expectString (Result.andThen (decodeResult decoder) >> toMsg)



-- Header


type Header
    = Header String String


header : String -> String -> Header
header =
    Header


makeHeader : Header -> Http.Header
makeHeader (Header name value) =
    Http.header name value



-- Body


type Body
    = EmptyBody
    | StringBody String String
    | JsonBody Value


emptyBody : Body
emptyBody =
    EmptyBody


stringBody : String -> String -> Body
stringBody =
    StringBody


jsonBody : Value -> Body
jsonBody =
    JsonBody


makeBody : Body -> Http.Body
makeBody body =
    case body of
        EmptyBody ->
            Http.emptyBody

        StringBody type_ value ->
            Http.stringBody type_ value

        JsonBody value ->
            Http.jsonBody value



-- Execute


run : Request msg -> Cmd msg
run (Request req) =
    Http.request
        { method = req.method
        , headers = List.map makeHeader req.headers
        , url = req.url
        , body = makeBody req.body
        , expect = makeExpect req.expect
        , timeout = req.timeout
        , tracker = req.tracker
        }



-- Handlers


{-| Internal
Equality EXCEPT expect
-}
requestEquals : Request x -> Request y -> Bool
requestEquals (Request r1) (Request r2) =
    (r1.method == r2.method)
        && (r1.headers == r2.headers)
        && (r1.url == r2.url)
        && (r1.body == r2.body)
        && (r1.timeout == r2.timeout)
        && (r1.tracker == r2.tracker)


handleGet : String -> Result Http.Error String -> Request msg -> Maybe msg
handleGet url =
    handleRequest <|
        get
            { url = url
            , expect = expectWhatever (\_ -> ())
            }


handleRequest : Request x -> Result Http.Error String -> Request msg -> Maybe msg
handleRequest r1 response ((Request req) as r2) =
    let
        (Expect toMsg) =
            req.expect
    in
    if requestEquals r1 r2 then
        Just (toMsg response)

    else
        Nothing
