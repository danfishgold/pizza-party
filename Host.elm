module Host exposing (..)

import Html exposing (Html, program, div, text)
import Config exposing (Config)
import Dict exposing (Dict)


type alias Topping =
    { name : String }


type alias User =
    { username : String }


type alias Preferences =
    List ( User, Topping )


type alias Model =
    { config : Config
    , users : List User
    , preferences : Preferences
    }


type Msg
    = Msg


validPieCount : Int -> List ( Topping, Int ) -> Bool
validPieCount slicesPerPizza slices =
    slices |> List.map Tuple.second |> List.sum |> \n -> n % slicesPerPizza == 0


validPartCount : Int -> ( Topping, List User ) -> Bool
validPartCount slicesPerPart ( _, slices ) =
    List.length slices % slicesPerPart == 0


validPartCounts : Int -> List ( User, Topping ) -> Bool
validPartCounts slicesPerPart preferences =
    toppingTallies preferences |> List.all (validPartCount slicesPerPart)


toppingTallies : Preferences -> List ( Topping, List User )
toppingTallies preferences =
    preferences
        |> tally Tuple.second Tuple.first


thing : Config -> Preferences -> List (List ( Topping, Int ))
thing { slicesPerPart, partsPerPie } preferences =
    preferences
        |> toppingTallies
        |> List.map (\( t, us ) -> ( t, List.length us ))
        |> List.map
            (\( t, n ) ->
                ( t
                , nearestWholes slicesPerPart n
                )
            )
        |> productReduce
        |> List.filter (validPieCount (slicesPerPart * partsPerPie))


nearestWholes : Int -> Int -> List Int
nearestWholes bin n =
    (if n % bin == 0 then
        [ n - 1, n, n + 1 ]
     else
        [ bin * floor (toFloat n / toFloat bin), ceiling (toFloat n / toFloat bin) ]
    )
        |> List.filter (\n -> n >= 0)


init : ( Model, Cmd Msg )
init =
    ( { config =
            { slicesPerPart = 2
            , partsPerPie = 4
            }
      , users = []
      , preferences = []
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Msg ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    thing model.config model.preferences
        |> List.map (\toppings -> div [] [ text <| toString toppings ])
        |> div []



-- MAIN


main : Program Never Model Msg
main =
    program
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }



-- PRODUCT


product : List ( key, List val ) -> List (List ( key, val ))
product options =
    case options of
        [] ->
            [ [] ]

        hd :: tl ->
            productCore hd (product tl)


productHelper : List ( key, List val ) -> List (List ( key, val )) -> List (List ( key, val ))
productHelper options partial =
    case options of
        [] ->
            List.map List.reverse partial

        hd :: tl ->
            productHelper tl (productCore hd partial)


productReduce : List ( key, List val ) -> List (List ( key, val ))
productReduce options =
    List.foldr productCore [ [] ] options


productCore : ( key, List val ) -> List (List ( key, val )) -> List (List ( key, val ))
productCore ( key, vals ) partial =
    let
        addToBeginning : val -> List ( key, val ) -> List ( key, val )
        addToBeginning val xs =
            ( key, val ) :: xs
    in
        List.concatMap (\val -> List.map (addToBeginning val) partial) vals



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


tallyReduce : (a -> comparable) -> (a -> value) -> List a -> Dict comparable (List value)
tallyReduce toKey toVal xs =
    List.foldl
        (\x ->
            Dict.update
                (toKey x)
                (Maybe.withDefault [] >> (::) (toVal x) >> Just)
        )
        Dict.empty
        xs
