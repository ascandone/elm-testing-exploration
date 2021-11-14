module Data.RemoteData exposing
    ( ReceivedToken
    , RemoteData(..)
    , onReceivedData
    )

import Http


type ReceivedToken
    = ReceivedToken


type RemoteData id value
    = NotAsked
    | Loading id
    | Received ReceivedToken (Result Http.Error value)


onReceivedData : id -> Result Http.Error value -> RemoteData id value -> RemoteData id value
onReceivedData expectedId receivedValue currentRemoteData =
    case currentRemoteData of
        NotAsked ->
            currentRemoteData

        Received _ _ ->
            currentRemoteData

        Loading loadingId ->
            if loadingId == expectedId then
                Received ReceivedToken receivedValue

            else
                currentRemoteData
