module Topping
    exposing
        ( Topping
        , Pair
        , Key
        , key
        , fromKey
        , all
        , decoder
        , encode
        , Count
        , countFromList
        , countFromKeyList
        , emptyCount
        , concatCounts
        )

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Count


type alias Topping =
    { name : String }


type alias Pair =
    ( Topping, Int )


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
    [ "Plain"
    , "Green Olives"
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


type alias Count =
    Count.Count Topping Key


countFromList : List ( Topping, Int ) -> Count
countFromList =
    Count.fromList key fromKey


countFromKeyList : List ( Key, Int ) -> Count
countFromKeyList =
    Count.fromKeyList key fromKey


emptyCount : Count
emptyCount =
    Count.empty key fromKey


concatCounts : List Count -> Count
concatCounts counts =
    List.foldl (Count.join) emptyCount counts
