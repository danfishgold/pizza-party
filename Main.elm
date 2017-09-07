module Main exposing (..)

import Html exposing (Html, program, div, button, text)
import Html.Attributes exposing (disabled)
import Html.Events exposing (onClick)
import Dict exposing (Dict)


type alias Topping =
    { name : String }


type alias User =
    { username : String }


type alias Preferences =
    List ( User, List Topping )


type alias Model =
    { slicesPerPart : Int
    , partsPerPie : Int
    , users : List User
    , counter : Int
    , preferences : Preferences
    }


type Msg
    = SetCounter Int


stablePies : Int -> Preferences -> Bool
stablePies slicesPerPizza preferences =
    preferences
        |> List.concatMap Tuple.second
        |> List.length
        |> \len -> len % slicesPerPizza == 0


stableToppingParts : Int -> ( Topping, List User ) -> Bool
stableToppingParts slicesPerPart ( _, slices ) =
    List.length slices % slicesPerPart == 0


toppingTallies : Preferences -> List ( Topping, List User )
toppingTallies preferences =
    preferences
        |> List.concatMap (\( user, toppings ) -> List.map (\t -> ( t, user )) toppings)
        |> tally Tuple.first Tuple.second


init : ( Model, Cmd Msg )
init =
    ( { slicesPerPart = 2
      , partsPerPie = 4
      , users = []
      , preferences = []
      , counter = 0
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetCounter value ->
            ( { model | counter = value }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    counter model.counter SetCounter


counter : Int -> (Int -> msg) -> Html msg
counter value toMsg =
    div []
        [ button
            [ if value == 0 then
                disabled True
              else
                onClick <| toMsg <| value - 1
            ]
            [ text "-" ]
        , text <| toString value
        , button [ onClick <| toMsg <| value + 1 ] [ text "+" ]
        ]



-- MAIN


main : Program Never Model Msg
main =
    program
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }



-- TALLY


tally : (a -> equatable) -> (a -> value) -> List a -> List ( equatable, List value )
tally toKey toVal xs =
    case xs of
        [] ->
            []

        hd :: tl ->
            let
                headKey =
                    toKey hd

                ( heads, others ) =
                    List.partition (toKey >> (==) headKey) tl
            in
                ( headKey, hd :: heads |> List.map toVal ) :: tally toKey toVal others


tallyHelper : (a -> equatable) -> (a -> value) -> List a -> List ( equatable, List value ) -> List ( equatable, List value )
tallyHelper toKey toVal xs partial =
    case xs of
        [] ->
            partial

        hd :: tl ->
            let
                headKey =
                    toKey hd

                ( heads, others ) =
                    List.partition (toKey >> (==) headKey) tl
            in
                tallyHelper toKey toVal others (( headKey, hd :: heads |> List.map toVal ) :: partial)


tallyDict : (a -> comparable) -> (a -> value) -> List a -> Dict comparable (List value)
tallyDict toKey toVal xs =
    case xs of
        [] ->
            Dict.empty

        hd :: tl ->
            tallyDict toKey toVal tl
                |> Dict.update (toKey hd) (Maybe.withDefault [] >> (::) (toVal hd) >> Just)


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
