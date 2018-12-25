module Config exposing (Config, Slices, Toppings)

import Topping exposing (BaseTopping)


type alias Config =
    { slices : Slices
    , toppings : Toppings
    }


type alias Slices =
    { slicesPerPart : Int
    , partsPerPie : Int
    }


type alias Toppings =
    { base : List BaseTopping
    , maxToppingsPerSlice : Int
    }
