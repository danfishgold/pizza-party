module Topping exposing (Topping, Key, key)


type alias Topping =
    { name : String }


type alias Key =
    String


key : Topping -> Key
key { name } =
    name
