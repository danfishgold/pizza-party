module RoomId exposing (RoomId, decoder, encode, fromString, toString)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type RoomId
    = RoomId String


fromString : String -> Result String RoomId
fromString str =
    if String.isEmpty str then
        Err "empty room id"

    else if String.toInt str == Nothing then
        Err "room id is not a number"

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
