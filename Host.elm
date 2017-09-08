module Host exposing (..)

import Html exposing (Html, program, div, h1, h2, span, text)
import Config exposing (Config)
import Dict exposing (Dict)
import Guest exposing (counter)


type alias Topping =
    String


type alias User =
    String


type alias UserPreferences =
    Dict ( User, Topping ) Int


type alias Preferences =
    List ( User, Topping )


type alias Model =
    { config : Config
    , userPrefs : UserPreferences
    }


type Msg
    = Add User Topping
    | Remove User Topping


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


thing : Config -> UserPreferences -> List ( List ( Topping, Int ), Int )
thing { slicesPerPart, partsPerPie } preferences =
    let
        asList : List ( Topping, Int )
        asList =
            preferences
                |> Dict.toList
                |> List.concatMap (\( ( u, t ), n ) -> List.repeat n ( u, t ))
                |> toppingTallies
                |> List.map (\( t, us ) -> ( t, List.length us ))
    in
        asList
            |> List.map
                (\( t, n ) -> ( t, nearestWholes slicesPerPart n ))
            |> productReduce
            |> List.filter (validPieCount (slicesPerPart * partsPerPie))
            |> List.map (\opt -> ( opt, distance opt asList ))
            |> List.sortBy Tuple.second


nearestWholes : Int -> Int -> List Int
nearestWholes bin n =
    (if n % bin == 0 then
        [ n - bin, n, n + bin ]
     else
        let
            fraction =
                (toFloat n / toFloat bin)
        in
            [ bin * floor fraction, bin * ceiling fraction ]
    )
        |> List.filter (\n -> n >= 0)


distance : List ( Topping, Int ) -> List ( Topping, Int ) -> Int
distance opt1 opt2 =
    let
        merger key n m val =
            val + abs (n - m)
    in
        Dict.merge
            (\k n dict -> merger k n 0 dict)
            (\k n m dict -> merger k n m dict)
            (\k m dict -> merger k 0 m dict)
            (Dict.fromList opt1)
            (Dict.fromList opt2)
            0


init : ( Model, Cmd Msg )
init =
    ( { config =
            { slicesPerPart = 2
            , partsPerPie = 4
            }
      , userPrefs = Dict.empty
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Add user topping ->
            ( { model | userPrefs = model.userPrefs |> Dict.update ( user, topping ) (modifyIntKey 1) }, Cmd.none )

        Remove user topping ->
            ( { model | userPrefs = model.userPrefs |> Dict.update ( user, topping ) (modifyIntKey -1) }, Cmd.none )


modifyIntKey : Int -> Maybe Int -> Maybe Int
modifyIntKey delta prev =
    case ( prev, delta > 0 ) of
        ( Nothing, True ) ->
            Just delta

        ( Nothing, False ) ->
            Nothing

        ( Just val, False ) ->
            if val + delta <= 0 then
                Nothing
            else
                Just (val + delta)

        ( Just val, True ) ->
            Just (val + delta)



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Users" ]
        , users |> List.map (userView Remove Add toppings model.userPrefs) |> div []
        , h1 [] [ text "Choices" ]
        , thing model.config model.userPrefs
            |> List.map (\toppings -> div [] [ text <| toString toppings ])
            |> div []
        ]


toppings : List Topping
toppings =
    [ "Olives", "Onion", "Tomato", "Pineapple", "Extra Cheese", "Corn", "Tuna" ]


users : List User
users =
    [ "Dan", "Sivan" ]


userView : (User -> Topping -> msg) -> (User -> Topping -> msg) -> List Topping -> UserPreferences -> User -> Html msg
userView decrease increase toppings prefs user =
    let
        counter value topping =
            toppingCounter
                (decrease user topping)
                (increase user topping)
                topping
                (Dict.get ( user, topping ) prefs |> Maybe.withDefault 0)
    in
        div []
            [ h2 [] [ text user ]
            , toppings |> List.map (counter 0) |> div []
            ]


toppingCounter : msg -> msg -> Topping -> Int -> Html msg
toppingCounter decrease increase topping value =
    div []
        [ text topping
        , counter value decrease increase
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



-- REMOVE SINGLE


removeSingle : a -> List a -> List a
removeSingle x xs =
    case xs of
        [] ->
            []

        hd :: tl ->
            if hd == x then
                tl
            else
                hd :: removeSingle x tl



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
                    List.partition (\item -> toKey item == headKey) tl
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
                    List.partition (\item -> toKey item == headKey) tl
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
