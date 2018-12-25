port module Socket exposing
    ( baseToppingListFromHost
    , baseToppingListRequestFromGuest
    , broadcastSliceTriplet
    , createRoomAsHost
    , createRoomResponseFromServer
    , findRoomAsGuest
    , guestDisconnectionsFromServer
    , hostDisconnectionsFromServer
    , kickGuestAsHost
    , kickedOutByHost
    , requestToppingsListFromHost
    , roomFoundResponseFromServer
    , sendBaseToppingListOrErrorToGuest
    , sliceTripletsFromGuest
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import RoomId exposing (RoomId)
import Topping exposing (BaseTopping, Topping)
import User exposing (User)



-- DECODERS AND ENCODERS


decodeTriplet : Decoder ( User, Topping, Int )
decodeTriplet =
    Decode.map3 (\a b c -> ( a, b, c ))
        (Decode.field "user" User.decoder)
        (Decode.field "topping" Topping.decoder)
        (Decode.field "count" Decode.int)


encodeTriplet : ( User, Topping, Int ) -> Encode.Value
encodeTriplet ( user, topping, count ) =
    Encode.object
        [ ( "user", User.encode user )
        , ( "topping", Topping.encode topping )
        , ( "count", Encode.int count )
        ]


decodeResult : Decode.Decoder (Result String a) -> Value -> Result String a
decodeResult decoder value =
    case Decode.decodeValue decoder value of
        Err error ->
            Err <| Decode.errorToString error

        Ok result ->
            result



-- CONNECTIONS


port createRoom : () -> Cmd msg


port createRoomResponse : (Value -> msg) -> Sub msg


port joinRoom : Value -> Cmd msg


port joinRoomResponse : (Value -> msg) -> Sub msg


createRoomAsHost : Cmd msg
createRoomAsHost =
    createRoom ()


createRoomResponseFromServer : (Result String RoomId -> msg) -> Sub msg
createRoomResponseFromServer toMsg =
    createRoomResponse
        (decodeResult
            (Decode.oneOf
                [ Decode.field "ok" (Decode.field "roomId" RoomId.decoder) |> Decode.map Ok
                , Decode.field "error" Decode.string |> Decode.map Err
                ]
            )
            >> toMsg
        )


findRoomAsGuest : RoomId -> Cmd msg
findRoomAsGuest roomId =
    joinRoom (RoomId.encode roomId)


roomFoundResponseFromServer : (Result String RoomId -> msg) -> Sub msg
roomFoundResponseFromServer toMsg =
    joinRoomResponse
        (decodeResult
            (Decode.oneOf
                [ Decode.field "ok" RoomId.decoder |> Decode.map Ok
                , Decode.field "error" Decode.string |> Decode.map Err
                ]
            )
            >> toMsg
        )



-- TRIPLET UPDATES


port sendTriplet : Value -> Cmd msg


port receiveTriplet : (Value -> msg) -> Sub msg


broadcastSliceTriplet : User -> Topping -> Int -> Cmd msg
broadcastSliceTriplet user topping value =
    encodeTriplet ( user, topping, value )
        |> sendTriplet


sliceTripletsFromGuest : (Maybe ( User, Topping, Int ) -> msg) -> Sub msg
sliceTripletsFromGuest toMsg =
    receiveTriplet
        (Decode.decodeValue decodeTriplet
            >> Result.mapError (Debug.log "error on topping triplet")
            >> Result.toMaybe
            >> toMsg
        )



-- NEW GUESTS


port requestBaseToppingList : Value -> Cmd msg


port receiveBaseToppingListRequest : (Value -> msg) -> Sub msg


port sendBaseToppingListOrError : Value -> Cmd msg


port receiveBaseToppingList : (Value -> msg) -> Sub msg


requestToppingsListFromHost : User -> Cmd msg
requestToppingsListFromHost user =
    requestBaseToppingList (User.encode user)


baseToppingListRequestFromGuest : (Result Decode.Error User -> msg) -> Sub msg
baseToppingListRequestFromGuest toMsg =
    receiveBaseToppingListRequest
        (Decode.decodeValue User.decoder >> toMsg)


sendBaseToppingListOrErrorToGuest : Result String (List BaseTopping) -> Cmd msg
sendBaseToppingListOrErrorToGuest result =
    case result of
        Ok toppings ->
            toppings
                |> Encode.list Topping.encodeBaseTopping
                |> (\toppingList ->
                        Encode.object [ ( "toppings", toppingList ) ]
                            |> sendBaseToppingListOrError
                   )

        Err error ->
            Encode.object [ ( "error", Encode.string error ) ]
                |> sendBaseToppingListOrError


baseToppingListFromHost : (Result String (List BaseTopping) -> msg) -> Sub msg
baseToppingListFromHost toMsg =
    receiveBaseToppingList
        (decodeResult
            (Decode.oneOf
                [ Decode.field "toppings" (Decode.list Topping.baseToppingDecoder) |> Decode.map Ok
                , Decode.field "error" Decode.string |> Decode.map Err
                ]
            )
            >> toMsg
        )



-- DISCONNECTIONS


port receiveGuestLeft : (Value -> msg) -> Sub msg


port receiveHostLeft : (Value -> msg) -> Sub msg


guestDisconnectionsFromServer : (Maybe User -> msg) -> Sub msg
guestDisconnectionsFromServer toMsg =
    receiveGuestLeft
        (Decode.decodeValue User.decoder
            >> Result.mapError (Debug.log "error on guest disconnection")
            >> Result.toMaybe
            >> toMsg
        )


hostDisconnectionsFromServer : msg -> Sub msg
hostDisconnectionsFromServer toMsg =
    receiveHostLeft (always toMsg)



-- KICKING OUT


port sendKickGuest : Value -> Cmd msg


port receiveKickGuest : (Value -> msg) -> Sub msg


kickGuestAsHost : User -> Cmd msg
kickGuestAsHost user =
    sendKickGuest (User.encode user)


kickedOutByHost : (Maybe User -> msg) -> Sub msg
kickedOutByHost toMsg =
    receiveKickGuest
        (Decode.decodeValue User.decoder
            >> Result.mapError (Debug.log "error on kicking out guest")
            >> Result.toMaybe
            >> toMsg
        )
