module Preferences exposing (..)

import Dict exposing (Dict)
import User exposing (User)
import Topping exposing (Topping)
import Count exposing (Count)


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


set : User -> Topping -> Int -> Preferences -> Preferences
set user topping value (Preferences prefs) =
    Dict.insert (key user topping) value prefs |> Preferences


add : User -> Topping -> Int -> Preferences -> ( Preferences, Int )
add user topping delta preferences =
    case ( get user topping preferences, delta > 0 ) of
        ( Nothing, True ) ->
            ( set user topping delta preferences, delta )

        ( Nothing, False ) ->
            ( preferences, 0 )

        ( Just val, False ) ->
            if val + delta < 0 then
                ( preferences, val )
            else
                ( set user topping (val + delta) preferences, val + delta )

        ( Just val, True ) ->
            ( set user topping (val + delta) preferences, val + delta )


toToppingCount : Preferences -> Topping.Count
toToppingCount (Preferences prefs) =
    prefs
        |> Dict.toList
        |> List.map (\( ( user, topping ), count ) -> ( topping, count ))
        |> Topping.countFromKeyList
