port module Socket exposing
    ( createRoom
    , joinRoom
    , kickOut
    , onGuestJoined
    , onGuestLeft
    , onHostLeft
    , onKickOut
    , onRoomCreated
    , onRoomJoined
    , onTripletUpdate
    , updateTriplet
    )

import Config exposing (Config)
import Error exposing (Error)
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)
import RoomId exposing (RoomId)
import ToppingTriplet as Triplet exposing (Triplet)
import User exposing (User)


{-| Nice decoder for server stuff

decodeResult Decode.int { error: "string error message" } => Err "string error message"
decodeResult Decode.int { "ok": 3 } => Ok 3
decodeResult Decode.int { "ok": "3" } => Err "Json decode error: whatever"

-}
decodeResult : Decode.Decoder a -> Value -> Result Error a
decodeResult successDecoder value =
    case
        Decode.decodeValue
            (Decode.oneOf
                [ Decode.field "ok" successDecoder |> Decode.map Ok
                , Decode.field "error" Error.decoder |> Decode.map Err
                ]
            )
            value
    of
        Err decodeError ->
            Err <| Error.JsonDecodeError <| Decode.errorToString decodeError

        Ok result ->
            result


encodeResult : (a -> Value) -> Result Error a -> Value
encodeResult encodeSuccess result =
    case result of
        Ok success ->
            Encode.object [ ( "ok", encodeSuccess success ) ]

        Err err ->
            Encode.object [ ( "error", Error.encode err ) ]



-- CREATE


port sendCreateRoom : Value -> Cmd msg


port receiveCreateRoomResponse : (Value -> msg) -> Sub msg


createRoom : Config -> Cmd msg
createRoom config =
    sendCreateRoom (Encode.object [ ( "config", Config.encode config ) ])


onRoomCreated : (Result Error RoomId -> msg) -> Sub msg
onRoomCreated toMsg =
    receiveCreateRoomResponse
        (decodeResult (Decode.field "roomId" RoomId.decoder)
            >> toMsg
        )



-- JOIN


port sendJoinRoom : Value -> Cmd msg


port receiveJoinRoomResponse : (Value -> msg) -> Sub msg


joinRoom : RoomId -> User -> Cmd msg
joinRoom roomId user =
    sendJoinRoom
        (Encode.object
            [ ( "roomId", RoomId.encode roomId )
            , ( "user", User.encode user )
            ]
        )


onRoomJoined : (Result Error Config -> msg) -> Sub msg
onRoomJoined toMsg =
    receiveJoinRoomResponse
        (decodeResult (Decode.field "config" Config.decoder) >> toMsg)



-- HOST & GUEST: TRIPLETS


port sendUpdateTriplet : Value -> Cmd msg


port receiveUpdateTriplet : (Value -> msg) -> Sub msg


updateTriplet : Triplet -> Cmd msg
updateTriplet triplet =
    sendUpdateTriplet (encodeResult Triplet.encode (Ok triplet))


onTripletUpdate : (Result Error Triplet -> msg) -> Sub msg
onTripletUpdate toMsg =
    receiveUpdateTriplet
        (decodeResult Triplet.decoder
            >> toMsg
        )



-- HOST & GUEST: CONNECTIONS & DISCONNECTIONS


port receiveGuestJoined : (Value -> msg) -> Sub msg


port receiveGuestLeft : (Value -> msg) -> Sub msg


port receiveHostLeft : (Value -> msg) -> Sub msg


onGuestJoined : (Result Error User -> msg) -> Sub msg
onGuestJoined toMsg =
    receiveGuestJoined
        (decodeResult (Decode.field "user" User.decoder) >> toMsg)


onGuestLeft : (Result Error User -> msg) -> Sub msg
onGuestLeft toMsg =
    receiveGuestLeft
        (decodeResult (Decode.field "user" User.decoder) >> toMsg)


onHostLeft : msg -> Sub msg
onHostLeft toMsg =
    receiveHostLeft (always toMsg)



-- HOST & GUEST: KICKING OUT


port sendKickGuest : Value -> Cmd msg


port receiveKickGuest : (Value -> msg) -> Sub msg


kickOut : User -> Cmd msg
kickOut user =
    sendKickGuest (encodeResult User.encode (Ok user))


onKickOut : (Result Error User -> msg) -> Sub msg
onKickOut toMsg =
    receiveKickGuest
        (decodeResult (Decode.field "user" User.decoder) >> toMsg)
