port module Host exposing (..)

import Html exposing (Html, program, div, h1, h2, span, text)
import Html.Attributes exposing (style)
import Config exposing (Config)
import Guest exposing (userView)
import Preferences as Pref exposing (Preferences)
import ToppingCount
import User exposing (User)
import Topping exposing (Topping)
import PizzaView
import Socket


type alias Model =
    { config : Config
    , userPrefs : Preferences
    }


type Msg
    = AddSliceCount Int User Topping
    | SetSliceCount Int User Topping
    | SendPreferences User
    | Noop


initialModel : Model
initialModel =
    { config =
        { slicesPerPart = 2
        , partsPerPie = 4
        }
    , userPrefs = Pref.empty
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Socket.requestsForAllCounts
            (Maybe.map SendPreferences
                >> Maybe.withDefault Noop
            )
        , Socket.toppingCountUpdates
            (Maybe.map (\( user, topping, count ) -> SetSliceCount count user topping)
                >> Maybe.withDefault Noop
            )
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddSliceCount delta user topping ->
            let
                ( newPrefs, newValue ) =
                    model.userPrefs |> Pref.add user topping delta
            in
                ( { model | userPrefs = newPrefs }
                , Socket.updateToppingCount user
                    topping
                    (newValue |> Maybe.withDefault 0)
                )

        SetSliceCount count user topping ->
            let
                newPrefs =
                    model.userPrefs |> Pref.set user topping count
            in
                ( { model | userPrefs = newPrefs }, Cmd.none )

        SendPreferences user ->
            ( model, Socket.sendAllCounts model.userPrefs )

        Noop ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ Pref.toToppingCount model.userPrefs
            |> PizzaView.pies 100 model.config
            |> div []
        , usersView (AddSliceCount -1) (AddSliceCount 1) Topping.all model.userPrefs users
        , h1 [] [ text "Changes" ]
        , Pref.toToppingCount model.userPrefs
            |> ToppingCount.stableOptions model.config
            |> List.map (\toppings -> div [] [ text <| toString toppings ])
            |> div []
        ]


users : List User
users =
    [ "Dan", "Sivan", "Ella", "Daniel", "Gali" ] |> List.map User


usersView : (User -> Topping -> msg) -> (User -> Topping -> msg) -> List Topping -> Preferences -> List User -> Html msg
usersView decrease increase toppings prefs users =
    let
        userDiv user =
            userView decrease increase toppings prefs user
    in
        div []
            [ h1 [] [ text "Users" ]
            , users
                |> List.map userDiv
                |> div
                    []
            ]


userView : (User -> Topping -> msg) -> (User -> Topping -> msg) -> List Topping -> Preferences -> User -> Html msg
userView decrease increase toppings prefs user =
    let
        value topping =
            (Pref.get user topping prefs |> Maybe.withDefault 0)
    in
        div []
            [ h2 [] [ text user.name ]
            , Guest.userView (decrease user) (increase user) value toppings prefs
            ]



-- MAIN


main : Program Never Model Msg
main =
    program
        { init = ( initialModel, Cmd.none )
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
