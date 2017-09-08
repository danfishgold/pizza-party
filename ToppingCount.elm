module ToppingCount exposing (..)

import Dict exposing (Dict)
import Topping exposing (Topping)


type alias ToppingCount =
    Dict Topping.Key Int


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
