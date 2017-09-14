module Preferences exposing (..)

import Dict exposing (Dict)
import User exposing (User)
import Topping exposing (Topping)
import ToppingCount exposing (ToppingCount)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type Preferences
    = Preferences (Dict ( User.Key, Topping.Key ) Int)


decoder : Decoder Preferences
decoder =
    decodeTriplet
        |> Decode.list
        |> Decode.map fromList


decodeTriplet : Decoder ( User, Topping, Int )
decodeTriplet =
    Decode.map3 (,,)
        (Decode.field "user" User.decoder)
        (Decode.field "topping" Topping.decoder)
        (Decode.field "count" Decode.int)


encode : Preferences -> Encode.Value
encode preferences =
    preferences |> toList |> List.map encodeTriplet |> Encode.list


encodeTriplet : ( User, Topping, Int ) -> Encode.Value
encodeTriplet ( user, topping, count ) =
    Encode.object
        [ ( "user", User.encode user )
        , ( "topping", Topping.encode topping )
        , ( "count", Encode.int count )
        ]


empty : Preferences
empty =
    Preferences Dict.empty


fromList : List ( User, Topping, Int ) -> Preferences
fromList =
    List.map (\( user, topping, count ) -> ( ( User.key user, Topping.key topping ), count ))
        >> Dict.fromList
        >> Preferences


toList : Preferences -> List ( User, Topping, Int )
toList =
    let
        parsePair ( ( userKey, toppingKey ), count ) =
            Maybe.map2 (\user topping -> ( user, topping, count ))
                (User.fromKey userKey)
                (Topping.fromKey toppingKey)
    in
        toDict
            >> Dict.toList
            >> List.filterMap parsePair


toDict : Preferences -> Dict ( User.Key, Topping.Key ) Int
toDict (Preferences prefs) =
    prefs


key : User -> Topping -> ( User.Key, Topping.Key )
key user topping =
    ( User.key user, Topping.key topping )


get : User -> Topping -> Preferences -> Maybe Int
get user topping (Preferences prefs) =
    Dict.get (key user topping) prefs


add : User -> Topping -> Int -> Preferences -> Preferences
add user topping delta (Preferences prefs) =
    let
        k =
            key user topping
    in
        case ( Dict.get k prefs, delta > 0 ) of
            ( Nothing, True ) ->
                Dict.insert k delta prefs |> Preferences

            ( Nothing, False ) ->
                Preferences prefs

            ( Just val, False ) ->
                if val + delta < 0 then
                    Preferences prefs
                else
                    Dict.insert k (val + delta) prefs |> Preferences

            ( Just val, True ) ->
                Dict.insert k (val + delta) prefs |> Preferences


toToppingCount : Preferences -> ToppingCount
toToppingCount (Preferences prefs) =
    Dict.foldl
        (\( _, topping ) n ->
            Dict.update topping (Maybe.withDefault 0 >> (+) n >> Just)
        )
        Dict.empty
        prefs
