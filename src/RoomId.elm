module RoomId exposing (RoomId, decoder, encode, fromString, toString)

import Error exposing (Error)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type RoomId
    = RoomId String


fromString : String -> Result Error RoomId
fromString str =
    if String.isEmpty str then
        Err Error.EmptyRoomId

    else if String.toInt str == Nothing then
        Err Error.RoomIdNotANumber

    else
        Ok (RoomId str)


toString : RoomId -> String
toString (RoomId str) =
    str


decoder : Decoder RoomId
decoder =
    Decode.map RoomId Decode.string


encode : RoomId -> Value
encode (RoomId str) =
    Encode.string str
