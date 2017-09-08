module Host exposing (..)

import Html exposing (Html, program, div, h1, h2, span, text)
import Config exposing (Config)
import Guest exposing (counter)
import Preferences as Pref exposing (Preferences)
import User exposing (User)
import Topping exposing (Topping)


type alias Model =
    { config : Config
    , userPrefs : Preferences
    }


type Msg
    = Add User Topping
    | Remove User Topping


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
        Add user topping ->
            ( { model | userPrefs = model.userPrefs |> Pref.add user topping 1 }, Cmd.none )

        Remove user topping ->
            ( { model | userPrefs = model.userPrefs |> Pref.add user topping -1 }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Users" ]
        , users |> List.map (userView Remove Add Topping.all model.userPrefs) |> div []
        , h1 [] [ text "Changes" ]
        , Pref.options model.config
            model.userPrefs
            |> List.map (\toppings -> div [] [ text <| toString toppings ])
            |> div []
        ]


users : List User
users =
    [ "Dan", "Sivan" ] |> List.map User


userView : (User -> Topping -> msg) -> (User -> Topping -> msg) -> List Topping -> Preferences -> User -> Html msg
userView decrease increase toppings prefs user =
    let
        counter value topping =
            toppingCounter
                (decrease user topping)
                (increase user topping)
                topping
                (Pref.get user topping prefs |> Maybe.withDefault 0)
    in
        div []
            [ h2 [] [ text user.name ]
            , toppings |> List.map (counter 0) |> div []
            ]


toppingCounter : msg -> msg -> Topping -> Int -> Html msg
toppingCounter decrease increase topping value =
    div []
        [ text topping.name
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
