module Guest exposing (..)

import Html exposing (Html, program, div, button, text)
import Html.Attributes exposing (disabled)
import Html.Events exposing (onClick)
import Topping exposing (Topping)
import User exposing (User)
import Preferences as Pref exposing (Preferences)


type alias Model =
    {}


type Msg
    = Msg


init : ( Model, Cmd Msg )
init =
    ( {}
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
    text ""


userView : (Topping -> msg) -> (Topping -> msg) -> (Topping -> Int) -> List Topping -> Preferences -> Html msg
userView decrease increase value toppings prefs =
    let
        counter topping =
            toppingCounter
                (decrease topping)
                (increase topping)
                (value topping)
                topping
    in
        List.map counter toppings |> div []


toppingCounter : msg -> msg -> Int -> Topping -> Html msg
toppingCounter decrease increase value topping =
    div []
        [ text topping.name
        , div []
            [ button
                [ if value == 0 then
                    disabled True
                  else
                    onClick <| decrease
                ]
                [ text "-" ]
            , text <| toString value
            , button [ onClick <| increase ] [ text "+" ]
            ]
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
