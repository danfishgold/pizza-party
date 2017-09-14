module User exposing (User, Key, key, fromKey, decoder, encode)

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
fromKey key =
    Decode.decodeString decoder key |> Result.toMaybe
