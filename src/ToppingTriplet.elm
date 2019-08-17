module ToppingTriplet exposing (Triplet, decoder, encode)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Topping exposing (Topping)
import User exposing (User)


type alias Triplet =
    { user : User
    , topping : Topping
    , count : Int
    }


decoder : Decoder Triplet
decoder =
    Decode.map3 Triplet
        (Decode.field "user" User.decoder)
        (Decode.field "topping" Topping.decoder)
        (Decode.field "count" Decode.int)


encode : Triplet -> Encode.Value
encode { user, topping, count } =
    Encode.object
        [ ( "user", User.encode user )
        , ( "topping", Topping.encode topping )
        , ( "count", Encode.int count )
        ]
