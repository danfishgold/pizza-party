module Topping exposing (Topping, Key, key, fromKey, all)


type alias Topping =
    { name : String }


type alias Key =
    String


key : Topping -> Key
key { name } =
    name


fromKey : Key -> Topping
fromKey name =
    { name = name }


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
