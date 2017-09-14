module Host exposing (..)

import Html exposing (Html, program, div, h1, h2, span, text)
import Html.Attributes exposing (style)
import Config exposing (Config)
import Guest exposing (userView)
import Preferences as Pref exposing (Preferences)
import ToppingCount
import User exposing (User)
import Topping exposing (Topping)
import PizzaView


type alias Model =
    { config : Config
    , userPrefs : Preferences
    }


type Msg
    = AddSlice User Topping
    | RemoveSlice User Topping


init : ( Model, Cmd Msg )
init =
    ( { config =
            { slicesPerPart = 2
            , partsPerPie = 4
            }
      , userPrefs = Pref.empty
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddSlice user topping ->
            ( { model | userPrefs = model.userPrefs |> Pref.add user topping 1 }, Cmd.none )

        RemoveSlice user topping ->
            ( { model | userPrefs = model.userPrefs |> Pref.add user topping -1 }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ PizzaView.pie 100 (model.config.slicesPerPart * model.config.partsPerPie) (Pref.toToppingCount model.userPrefs)
        , usersView RemoveSlice AddSlice Topping.all model.userPrefs users
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
            div
                [ style
                    [ ( "grid-row-start", "1" )
                    , ( "grid-row-end", "2" )
                    ]
                ]
                [ userView decrease increase toppings prefs user ]
    in
        div []
            [ h1 [] [ text "Users" ]
            , users
                |> List.map userDiv
                |> div
                    [ style
                        [ ( "display", "grid" )
                        , ( "grid-auto-columns", "1fr" )
                        , ( "grid-auto-rows", "auto" )
                        ]
                    ]
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
