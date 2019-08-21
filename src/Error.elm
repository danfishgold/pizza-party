module Error exposing (Error(..), decoder, encode, toString)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type Error
    = JsonDecodeError String
    | NetworkError String
    | EmptyRoomId
    | RoomIdNotANumber
    | NoRoomWithId
    | EmptyUsername
    | ExistingUsername
    | HostLeft
    | YouWereKickedOut
    | AlreadyHost


decoder : Decoder Error
decoder =
    Decode.oneOf
        [ Decode.map2 (\_ err -> JsonDecodeError err)
            (exactly "json-decode" ())
            Decode.string
        , Decode.map2 (\_ err -> NetworkError err)
            (exactly "network-error" ())
            Decode.string
        , exactly "empty-room-id" EmptyRoomId
        , exactly "room-id-not-a-number" RoomIdNotANumber
        , exactly "no-room-with-id" NoRoomWithId
        , exactly "empty-username" EmptyUsername
        , exactly "existing-username" ExistingUsername
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


encode : Error -> Encode.Value
encode error =
    case error of
        JsonDecodeError err ->
            Encode.object
                [ ( "type", Encode.string "json-decode" )
                , ( "payload", Encode.string err )
                ]

        NetworkError err ->
            Encode.object
                [ ( "type", Encode.string "network-error" )
                , ( "payload", Encode.string err )
                ]

        EmptyRoomId ->
            Encode.string "empty-room-id"

        RoomIdNotANumber ->
            Encode.string "room-id-not-a-number"

        NoRoomWithId ->
            Encode.string "no-room-with-id"

        EmptyUsername ->
            Encode.string "empty-username"

        ExistingUsername ->
            Encode.string "existing-username"

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

        RoomIdNotANumber ->
            "that's not a valid party id"

        NoRoomWithId ->
            "there's no party with this id"

        EmptyUsername ->
            "please enter your name"

        ExistingUsername ->
            "there's already someone with that name at this party. please be more specific"

        HostLeft ->
            "the host left the party. awkward"

        YouWereKickedOut ->
            "you were kicked out"

        AlreadyHost ->
            "you're already hosting a party"
