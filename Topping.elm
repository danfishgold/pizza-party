module Topping exposing (Topping, Key, key, all)


type alias Topping =
    { name : String }


type alias Key =
    String


key : Topping -> Key
key { name } =
    name


all : List Topping
all =
    [ "Green Olives"
    , "Black Olives"
    , "Mushrooms"
    , "Onion"
    , "Corn"
    , "Tomato"
    , "Pineapple"
    , "Peperoni"
    , "Extra Cheese"
    , "Tuna"
    ]
        |> List.map Topping
