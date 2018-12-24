module RoomId exposing (RoomId, decoder, encode, fromString, toString)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type RoomId
    = RoomId String


fromString : String -> RoomId
fromString str =
    RoomId str


toString : RoomId -> String
toString (RoomId str) =
    str


decoder : Decoder RoomId
decoder =
    Decode.map RoomId Decode.string


encode : RoomId -> Value
encode (RoomId str) =
    Encode.string str
