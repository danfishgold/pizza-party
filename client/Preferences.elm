module Preferences exposing (..)

import Dict exposing (Dict)
import User exposing (User)
import Topping exposing (Topping)
import ToppingCount exposing (ToppingCount)
import Config exposing (Config)


type Preferences
    = Preferences (Dict ( User.Key, Topping.Key ) Int)


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
