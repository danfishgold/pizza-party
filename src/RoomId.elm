module RoomId exposing (RoomId, decoder, encode, fromString, toString)

import Error exposing (Error)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type RoomId
    = RoomId Int


fromString : String -> Result Error RoomId
fromString str =
    if String.isEmpty str then
        Err Error.EmptyRoomId

    else
        case String.toInt str of
            Nothing ->
                Err (Error.RoomIdNotANumber str)

            Just id ->
                Ok (RoomId id)


toString : RoomId -> String
toString (RoomId id) =
    String.fromInt id


decoder : Decoder RoomId
decoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case fromString str of
                    Err err ->
                        Decode.fail (Error.toString err)

                    Ok id ->
                        Decode.succeed id
            )


encode : RoomId -> Value
encode (RoomId id) =
    Encode.string (String.fromInt id)
