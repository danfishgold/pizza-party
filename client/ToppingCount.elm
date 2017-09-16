module ToppingCount exposing (..)

import Dict exposing (Dict)
import Topping exposing (Topping)
import Config exposing (Config)


type alias ToppingCount =
    Dict Topping.Key Int


type alias Pair =
    ( Topping, Int )


stableOptions : Config -> ToppingCount -> List ToppingCount
stableOptions { slicesPerPart, partsPerPie } toppingCount =
    toppingCount
        |> Dict.map (always (nearestWholes slicesPerPart))
        |> dictProduct
        |> List.filter (validPieCount (slicesPerPart * partsPerPie))
        |> List.filter (sliceCount >> (/=) 0)
        |> List.map (\opt -> subtract opt toppingCount)
        |> List.sortBy sliceCount


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



-- DICT Product


dictProduct : Dict comparable (List val) -> List (Dict comparable val)
dictProduct optionDict =
    Dict.foldl dictProductCore [ Dict.empty ] optionDict


dictProductCore : comparable -> List val -> List (Dict comparable val) -> List (Dict comparable val)
dictProductCore key vals partial =
    List.concatMap (\val -> List.map (Dict.insert key val) partial) vals



-- TALLY


tally : (a -> comparable) -> (a -> value) -> List a -> Dict comparable (List value)
tally toKey toVal xs =
    tallyHelperDict toKey toVal xs Dict.empty


tallyHelperDict : (a -> comparable) -> (a -> value) -> List a -> Dict comparable (List value) -> Dict comparable (List value)
tallyHelperDict toKey toVal xs partial =
    case xs of
        [] ->
            partial

        hd :: tl ->
            (tallyHelperDict toKey toVal tl)
                (partial
                    |> Dict.update (toKey hd) (Maybe.withDefault [] >> (::) (toVal hd) >> Just)
                )


tallyReduce : (a -> comparable) -> (a -> value) -> List a -> Dict comparable (List value)
tallyReduce toKey toVal xs =
    List.foldl
        (\x ->
            Dict.update
                (toKey x)
                (Maybe.withDefault [] >> (::) (toVal x) >> Just)
        )
        Dict.empty
        xs
