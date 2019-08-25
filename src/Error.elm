module Error exposing (Error(..), decoder, encode, toString)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type Error
    = JsonDecodeError String
    | NetworkError String
    | RoomIdNotANumber String
    | NoRoomWithId String
    | ExistingUsername String
    | EmptyRoomId
    | EmptyUsername
    | HostLeft
    | YouWereKickedOut
    | AlreadyHost


decoder : Decoder Error
decoder =
    Decode.oneOf
        [ decodeWithPayload "json-decode" Decode.string JsonDecodeError
        , decodeWithPayload "network-error" Decode.string NetworkError
        , decodeWithPayload "room-id-not-a-number" Decode.string RoomIdNotANumber
        , decodeWithPayload "no-room-with-id" Decode.string NoRoomWithId
        , decodeWithPayload "existing-username" Decode.string ExistingUsername
        , exactly "empty-room-id" EmptyRoomId
        , exactly "empty-username" EmptyUsername
        , exactly "host-left" HostLeft
        , exactly "you-were-kicked-out" YouWereKickedOut
        , exactly "already-host" AlreadyHost
        ]


exactly : String -> a -> Decoder a
exactly str success =
    Decode.string
        |> Decode.andThen
            (\parsed ->
                if parsed == str then
                    Decode.succeed success

                else
                    Decode.fail ("Expected " ++ str ++ ", but got " ++ parsed)
            )


decodeWithPayload : String -> Decoder payload -> (payload -> Error) -> Decoder Error
decodeWithPayload type_ payloadDecoder toError =
    Decode.map2 (always toError)
        (Decode.field "type" <| exactly type_ ())
        (Decode.field "payload" payloadDecoder)


encodeWithPayload : String -> Encode.Value -> Encode.Value
encodeWithPayload type_ encodedPayload =
    Encode.object
        [ ( "type", Encode.string type_ )
        , ( "payload", encodedPayload )
        ]


encode : Error -> Encode.Value
encode error =
    case error of
        JsonDecodeError err ->
            encodeWithPayload "json-decode" (Encode.string err)

        NetworkError err ->
            encodeWithPayload "network-error" (Encode.string err)

        RoomIdNotANumber roomString ->
            encodeWithPayload "room-id-not-a-number" (Encode.string roomString)

        NoRoomWithId roomId ->
            encodeWithPayload "no-room-with-id" (Encode.string roomId)

        ExistingUsername username ->
            encodeWithPayload "existing-username" (Encode.string username)

        EmptyRoomId ->
            Encode.string "empty-room-id"

        EmptyUsername ->
            Encode.string "empty-username"

        HostLeft ->
            Encode.string "host-left"

        YouWereKickedOut ->
            Encode.string "you-were-kicked-out"

        AlreadyHost ->
            Encode.string "already-host"


toString : Error -> String
toString error =
    case error of
        JsonDecodeError err ->
            "json decoding error: " ++ err

        NetworkError err ->
            "network error: " ++ err

        EmptyRoomId ->
            "please enter a party id"

        RoomIdNotANumber roomString ->
            "that's not a valid party id"

        NoRoomWithId roomId ->
            "there's no party with this id"

        EmptyUsername ->
            "please enter your name"

        ExistingUsername username ->
            "there's already someone with that name at this party. please be more specific"

        HostLeft ->
            "the host left the party. awkward"

        YouWereKickedOut ->
            "you were kicked out"

        AlreadyHost ->
            "you're already hosting a party"
