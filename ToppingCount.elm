module ToppingCount exposing (..)

import Dict exposing (Dict)
import Topping exposing (Topping)
import Config exposing (Config)


type alias ToppingCount =
    Dict Topping.Key Int


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



-- DICT Product


dictProduct : Dict comparable (List val) -> List (Dict comparable val)
dictProduct optionDict =
    Dict.foldl dictProductCore [ Dict.empty ] optionDict


dictProductCore : comparable -> List val -> List (Dict comparable val) -> List (Dict comparable val)
dictProductCore key vals partial =
    List.concatMap (\val -> List.map (Dict.insert key val) partial) vals
