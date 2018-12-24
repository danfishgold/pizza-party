module User exposing (Key, User, decoder, encode, fromKey, key)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type alias User =
    { name : String }


decoder : Decoder User
decoder =
    Decode.string
        |> Decode.map User


encode : User -> Encode.Value
encode { name } =
    Encode.string name


type alias Key =
    String


key : User -> Key
key user =
    Encode.encode 0 (encode user)


fromKey : Key -> Maybe User
fromKey key_ =
    Decode.decodeString decoder key_ |> Result.toMaybe
