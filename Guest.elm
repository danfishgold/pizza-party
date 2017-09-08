module Guest exposing (..)

import Html exposing (Html, program, div, button, text)
import Html.Attributes exposing (disabled)
import Html.Events exposing (onClick)


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
