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


options : Config -> Preferences -> List ToppingCount
options { slicesPerPart, partsPerPie } preferences =
    let
        toppingCount =
            toToppingCount preferences
    in
        toppingCount
            |> Dict.map (always (nearestWholes slicesPerPart))
            |> dictProduct
            |> List.filter (ToppingCount.validPieCount (slicesPerPart * partsPerPie))
            |> List.map (\opt -> ToppingCount.subtract opt toppingCount)
            |> List.sortBy ToppingCount.sliceCount


nearestWholes : Int -> Int -> List Int
nearestWholes bin n =
    (if n % bin == 0 then
        [ n - bin, n, n + bin ]
     else
        let
            fraction =
                (toFloat n / toFloat bin)
        in
            [ bin * floor fraction, bin * ceiling fraction ]
    )
        |> List.filter (\n -> n >= 0)



-- PRODUCT


product : List ( key, List val ) -> List (List ( key, val ))
product options =
    case options of
        [] ->
            [ [] ]

        hd :: tl ->
            productCore hd (product tl)


productHelper : List ( key, List val ) -> List (List ( key, val )) -> List (List ( key, val ))
productHelper options partial =
    case options of
        [] ->
            List.map List.reverse partial

        hd :: tl ->
            productHelper tl (productCore hd partial)


productReduce : List ( key, List val ) -> List (List ( key, val ))
productReduce options =
    List.foldr productCore [ [] ] options


productCore : ( key, List val ) -> List (List ( key, val )) -> List (List ( key, val ))
productCore ( key, vals ) partial =
    let
        addToBeginning : val -> List ( key, val ) -> List ( key, val )
        addToBeginning val xs =
            ( key, val ) :: xs
    in
        List.concatMap (\val -> List.map (addToBeginning val) partial) vals



-- DICT Product


dictProduct : Dict comparable (List val) -> List (Dict comparable val)
dictProduct optionDict =
    Dict.foldl dictProductCore [ Dict.empty ] optionDict


dictProductCore : comparable -> List val -> List (Dict comparable val) -> List (Dict comparable val)
dictProductCore key vals partial =
    List.concatMap (\val -> List.map (Dict.insert key val) partial) vals
