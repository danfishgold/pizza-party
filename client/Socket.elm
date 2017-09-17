port module Socket
    exposing
        ( State(..)
        , mapState
        , requestHostConnectionFromServer
        , hostConnectionResponseFromServer
        , broadcastSliceTriplet
        , sliceTripletsFromGuest
        , requestToppingsListFromHost
        , toppingListRequestFromGuest
        , sendToppingListOrErrorToGuest
        , toppingListFromHost
        )

import Json.Encode as Encode exposing (Value)
import Json.Decode as Decode exposing (Decoder)
import User exposing (User)
import Topping exposing (Topping)


-- STATE


type State a
    = NotRequested
    | Joining
    | Joined a
    | Denied String


stateFromResult : Result String a -> State a
stateFromResult result =
    case result of
        Err error ->
            Denied error

        Ok a ->
            Joined a


mapState : (a -> b) -> State a -> State b
mapState fn state =
    case state of
        NotRequested ->
            NotRequested

        Joining ->
            Joining

        Joined a ->
            Joined (fn a)

        Denied err ->
            Denied err



-- DECODERS AND ENCODERS


decodeTriplet : Decoder ( User, Topping, Int )
decodeTriplet =
    Decode.map3 (,,)
        (Decode.field "user" User.decoder)
        (Decode.field "topping" Topping.decoder)
        (Decode.field "count" Decode.int)


encodeTriplet : ( User, Topping, Int ) -> Encode.Value
encodeTriplet ( user, topping, count ) =
    Encode.object
        [ ( "user", User.encode user )
        , ( "topping", Topping.encode topping )
        , ( "count", Encode.int count )
        ]


decodeResult : Decode.Decoder (Result String a) -> Value -> Result String a
decodeResult decoder value =
    case Decode.decodeValue decoder value of
        Err error ->
            Err error

        Ok result ->
            result



-- HOST CONNECTION


port connectAsHost : () -> Cmd msg


port connectAsHostResponse : (Value -> msg) -> Sub msg


requestHostConnectionFromServer : Cmd msg
requestHostConnectionFromServer =
    connectAsHost ()


hostConnectionResponseFromServer : (State () -> msg) -> Sub msg
hostConnectionResponseFromServer toMsg =
    connectAsHostResponse
        (decodeResult
            (Decode.oneOf
                [ Decode.field "ok" (Decode.succeed ()) |> Decode.map Ok
                , Decode.field "error" Decode.string |> Decode.map Err
                ]
            )
            >> stateFromResult
            >> toMsg
        )



-- TRIPLET UPDATES


port sendTriplet : Value -> Cmd msg


port receiveTriplet : (Value -> msg) -> Sub msg


broadcastSliceTriplet : User -> Topping -> Int -> Cmd msg
broadcastSliceTriplet user topping value =
    encodeTriplet ( user, topping, value )
        |> sendTriplet


sliceTripletsFromGuest : (Maybe ( User, Topping, Int ) -> msg) -> Sub msg
sliceTripletsFromGuest toMsg =
    receiveTriplet
        (Decode.decodeValue decodeTriplet
            >> Result.mapError (Debug.log "error on topping triplet")
            >> Result.toMaybe
            >> toMsg
        )



-- NEW GUESTS


port requestToppingList : Value -> Cmd msg


port receiveToppingListRequest : (Value -> msg) -> Sub msg


port sendToppingListOrError : Value -> Cmd msg


port receiveToppingList : (Value -> msg) -> Sub msg


requestToppingsListFromHost : User -> Cmd msg
requestToppingsListFromHost user =
    requestToppingList (User.encode user)


toppingListRequestFromGuest : (Result String User -> msg) -> Sub msg
toppingListRequestFromGuest toMsg =
    receiveToppingListRequest
        (Decode.decodeValue User.decoder >> toMsg)


sendToppingListOrErrorToGuest : Result String (List Topping) -> Cmd msg
sendToppingListOrErrorToGuest result =
    case result of
        Ok toppings ->
            toppings
                |> List.map Topping.encode
                |> Encode.list
                |> \toppingList ->
                    Encode.object [ ( "toppings", toppingList ) ]
                        |> sendToppingListOrError

        Err error ->
            Encode.object [ ( "error", Encode.string error ) ]
                |> sendToppingListOrError


toppingListFromHost : (State (List Topping) -> msg) -> Sub msg
toppingListFromHost toMsg =
    receiveToppingList
        (decodeResult
            (Decode.oneOf
                [ Decode.field "toppings" (Decode.list Topping.decoder) |> Decode.map Ok
                , Decode.field "error" Decode.string |> Decode.map Err
                ]
            )
            >> stateFromResult
            >> toMsg
        )
