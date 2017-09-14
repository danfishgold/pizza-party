module Topping exposing (Topping, Key, key, fromKey, all, decoder, encode)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type alias Topping =
    { name : String }


decoder : Decoder Topping
decoder =
    Decode.string |> Decode.map Topping


encode : Topping -> Encode.Value
encode { name } =
    Encode.string name


type alias Key =
    String


key : Topping -> Key
key topping =
    Encode.encode 0 (encode topping)


fromKey : Key -> Maybe Topping
fromKey key =
    Decode.decodeString decoder key |> Result.toMaybe


all : List Topping
all =
    [ "Green Olives"
    , "Black Olives"
    , "Mushrooms"
    , "Onion"
    , "Corn"
    , "Tomato"
    , "Pineapple"
    , "Peperoni"
    , "Extra Cheese"
    , "Tuna"
    ]
        |> List.map Topping
