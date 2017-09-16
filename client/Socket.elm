port module Socket
    exposing
        ( State(..)
        , connectAsHost
        , requestsForAllCounts
        , toppingCountUpdates
        , updateToppingCount
        , sendAllCounts
        )

import Json.Encode exposing (Value)
import Json.Decode
import Preferences exposing (Preferences)
import User exposing (User)
import Topping exposing (Topping)


type State a
    = NotRequested
    | Joining
    | Joined a
    | Denied String


port connectAsHost : Bool -> Cmd msg


port sendToppingTriplet : Value -> Cmd msg


port receiveToppingTriplet : (Value -> msg) -> Sub msg


port sendAllToppingCounts : Value -> Cmd msg


port receiveRequestForAllToppingCounts : (Value -> msg) -> Sub msg


requestsForAllCounts : (Maybe User -> msg) -> Sub msg
requestsForAllCounts toMsg =
    receiveRequestForAllToppingCounts
        (Json.Decode.decodeValue User.decoder
            >> Result.mapError (Debug.log "error on all topping counts")
            >> Result.toMaybe
            >> toMsg
        )


toppingCountUpdates : (Maybe ( User, Topping, Int ) -> msg) -> Sub msg
toppingCountUpdates toMsg =
    receiveToppingTriplet
        (Json.Decode.decodeValue Preferences.decodeTriplet
            >> Result.mapError (Debug.log "error on topping triplet")
            >> Result.toMaybe
            >> toMsg
        )


updateToppingCount : User -> Topping -> Int -> Cmd msg
updateToppingCount user topping value =
    Preferences.encodeTriplet ( user, topping, value )
        |> sendToppingTriplet


sendAllCounts : Preferences -> Cmd msg
sendAllCounts prefs =
    sendAllToppingCounts (Preferences.encode prefs)
