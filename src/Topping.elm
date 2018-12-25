module Topping exposing
    ( BaseTopping
    , Count
    , Key
    , Pair
    , Topping
    , all
    , baseToppingDecoder
    , concatCounts
    , countFromList
    , decoder
    , emptyCount
    , encode
    , encodeBaseTopping
    , filterZeros
    , fromBase
    , toSortedList
    , toString
    )

import Count
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type alias BaseTopping =
    { name : String }


type alias Topping =
    { parts : List BaseTopping
    }


type alias Pair =
    ( Topping, Int )


baseToppingDecoder : Decoder BaseTopping
baseToppingDecoder =
    Decode.string |> Decode.map BaseTopping


encodeBaseTopping : BaseTopping -> Encode.Value
encodeBaseTopping { name } =
    Encode.string name


decoder : Decoder Topping
decoder =
    Decode.list baseToppingDecoder |> Decode.map Topping


encode : Topping -> Encode.Value
encode { parts } =
    Encode.list encodeBaseTopping parts


type alias Key =
    String


key : Topping -> Key
key topping =
    Encode.encode 0 (encode topping)


fromKey : Key -> Maybe Topping
fromKey key_ =
    Decode.decodeString decoder key_ |> Result.toMaybe


fromBase : BaseTopping -> Topping
fromBase base =
    { parts = [ base ] }


all : List BaseTopping
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
        |> List.map BaseTopping


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
    List.foldl Count.join emptyCount counts


toString : Topping -> String
toString { parts } =
    List.map .name parts |> String.join " + "


toSortedList : List BaseTopping -> Count -> List Topping
toSortedList baseToppings count =
    let
        simple =
            List.map fromBase baseToppings
                |> List.sortBy toString

        extras =
            Count.keys count
                |> List.filter (\top -> not <| List.member top simple)
                |> List.sortBy toString
    in
    simple ++ extras


filterZeros : Count -> Count
filterZeros count =
    let
        filterer topping cnt =
            cnt > 0 || List.length topping.parts == 1
    in
    Count.filter filterer count
