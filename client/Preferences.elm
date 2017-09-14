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
    Decode.map3 (\user topping count -> ( ( user, topping ), count ))
        (Decode.field "user" User.decoder |> Decode.map User.key)
        (Decode.field "topping" Topping.decoder |> Decode.map Topping.key)
        (Decode.field "count" Decode.int)
        |> Decode.list
        |> Decode.map (Dict.fromList)
        |> Decode.map Preferences


encode : Preferences -> Encode.Value
encode (Preferences prefs) =
    let
        encodePair ( ( userKey, toppingKey ), count ) =
            case ( User.fromKey userKey, Topping.fromKey toppingKey ) of
                ( Just user, Just topping ) ->
                    Encode.object
                        [ ( "user", User.encode user )
                        , ( "topping", Topping.encode user )
                        , ( "count", Encode.int count )
                        ]
                        |> Just

                _ ->
                    Nothing
    in
        prefs |> Dict.toList |> List.filterMap encodePair |> Encode.list


empty : Preferences
empty =
    Preferences Dict.empty


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
