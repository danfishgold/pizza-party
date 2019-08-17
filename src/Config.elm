module Config exposing (Config, Slices, Toppings, decoder, default, encode)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Topping exposing (BaseTopping)


type alias Config =
    { slices : Slices
    , toppings : Toppings
    }


default : Config
default =
    { slices =
        { slicesPerPart = 2
        , partsPerPie = 4
        }
    , toppings =
        { base = Topping.all
        , maxToppingsPerSlice = 1
        }
    }


type alias Slices =
    { slicesPerPart : Int
    , partsPerPie : Int
    }


type alias Toppings =
    { base : List BaseTopping
    , maxToppingsPerSlice : Int
    }


decoder : Decoder Config
decoder =
    Decode.map2 Config
        (Decode.field "slices" slicesDecoder)
        (Decode.field "toppings" toppingsDecoder)


slicesDecoder : Decoder Slices
slicesDecoder =
    Decode.map2 Slices
        (Decode.field "slicesPerPart" Decode.int)
        (Decode.field "partsPerPie" Decode.int)


toppingsDecoder : Decoder Toppings
toppingsDecoder =
    Decode.map2 Toppings
        (Decode.field "base" <| Decode.list Topping.baseToppingDecoder)
        (Decode.field "maxToppingsPerSlice" Decode.int)


encode : Config -> Encode.Value
encode config =
    Encode.object
        [ ( "slices", encodeSlices config.slices )
        , ( "toppings", encodeToppings config.toppings )
        ]


encodeSlices : Slices -> Encode.Value
encodeSlices slices =
    Encode.object
        [ ( "slicesPerPart", Encode.int slices.slicesPerPart )
        , ( "partsPerPie", Encode.int slices.partsPerPie )
        ]


encodeToppings : Toppings -> Encode.Value
encodeToppings toppings =
    Encode.object
        [ ( "base", Encode.list Topping.encodeBaseTopping toppings.base )
        , ( "maxToppingsPerSlice", Encode.int toppings.maxToppingsPerSlice )
        ]
