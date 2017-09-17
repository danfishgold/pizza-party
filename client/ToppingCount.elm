module ToppingCount exposing (..)

import Dict exposing (Dict)
import Topping exposing (Topping)


type alias ToppingCount =
    Dict Topping.Key Int


type alias Pair =
    ( Topping, Int )


subtract : ToppingCount -> ToppingCount -> ToppingCount
subtract opt1 opt2 =
    let
        merger key n m diffDict =
            if n == m then
                diffDict
            else
                Dict.insert key (n - m) diffDict
    in
        Dict.merge
            (\k n dict -> merger k n 0 dict)
            (\k n m dict -> merger k n m dict)
            (\k m dict -> merger k 0 m dict)
            opt1
            opt2
            Dict.empty


sliceCount : ToppingCount -> Int
sliceCount toppingCount =
    Dict.foldl (\topping count sum -> sum + abs count) 0 toppingCount


validPieCount : Int -> ToppingCount -> Bool
validPieCount slicesPerPie toppingCount =
    sliceCount toppingCount % slicesPerPie == 0


toList : ToppingCount -> List Pair
toList =
    let
        parsePair ( toppingKey, count ) =
            Maybe.map (flip (,) count)
                (Topping.fromKey toppingKey)
    in
        Dict.toList
            >> List.filterMap parsePair


fromList : List Pair -> ToppingCount
fromList =
    tally (Tuple.first >> Topping.key) Tuple.second
        >> Dict.map (always List.sum)


empty : ToppingCount
empty =
    Dict.empty


get : Topping -> ToppingCount -> Int
get topping counts =
    Dict.get (Topping.key topping) counts |> Maybe.withDefault 0


set : Topping -> Int -> ToppingCount -> ToppingCount
set topping value counts =
    Dict.insert (Topping.key topping) value counts


add : Topping -> Int -> ToppingCount -> ( ToppingCount, Int )
add topping delta counts =
    case ( get topping counts, delta > 0 ) of
        ( 0, True ) ->
            ( set topping delta counts, delta )

        ( 0, False ) ->
            ( counts, 0 )

        ( val, False ) ->
            if val + delta < 0 then
                ( counts, val )
            else
                ( set topping (val + delta) counts, val + delta )

        ( val, True ) ->
            ( set topping (val + delta) counts, val + delta )


tally : (a -> comparable) -> (a -> value) -> List a -> Dict comparable (List value)
tally toKey toVal xs =
    List.foldl
        (\x ->
            Dict.update
                (toKey x)
                (Maybe.withDefault [] >> (::) (toVal x) >> Just)
        )
        Dict.empty
        xs
