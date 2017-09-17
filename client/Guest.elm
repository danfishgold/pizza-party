module Guest exposing (..)

import Html exposing (Html, program, div, input, button, text, span)
import Html.Attributes exposing (disabled, style, value)
import Html.Events exposing (onClick, onInput)
import Topping exposing (Topping)
import Count
import User exposing (User)
import Socket exposing (State(..))


type alias Model =
    { user : User
    , group : Socket.State Group
    , counts : Topping.Count
    }


type alias Group =
    { toppings : List Topping
    }


type Msg
    = EditName String
    | SetGroupState (Socket.State Group)
    | AddSliceCount Int Topping
    | Noop


initialModel : Model
initialModel =
    { user = { name = "" }
    , group = NotRequested
    , counts = Topping.emptyCount
    }


fake : Model
fake =
    { initialModel
        | user = { name = "fake" }
        , group = Joined { toppings = Topping.all }
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.group of
        Joining ->
            Socket.toppingListFromHost (Socket.mapState Group >> SetGroupState)

        _ ->
            Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EditName name ->
            ( { model | user = { name = name } }, Cmd.none )

        SetGroupState state ->
            let
                command =
                    case state of
                        Joining ->
                            Socket.requestToppingsListFromHost model.user

                        _ ->
                            Cmd.none
            in
                ( { model | group = state }, command )

        AddSliceCount delta topping ->
            let
                ( newCounts, newValue ) =
                    model.counts |> Count.add topping delta
            in
                ( { model | counts = newCounts }
                , Socket.broadcastSliceTriplet model.user topping newValue
                )

        Noop ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    case model.group of
        NotRequested ->
            div []
                [ text "Enter your name"
                , input
                    [ onInput EditName
                    , value model.user.name
                    ]
                    []
                , button
                    [ onClick (SetGroupState Joining)
                    , disabled (String.isEmpty model.user.name)
                    ]
                    [ text "Join" ]
                ]

        Joining ->
            text "Joining..."

        Joined { toppings } ->
            userView
                (AddSliceCount -1)
                (AddSliceCount 1)
                (flip Count.get model.counts)
                toppings

        Denied error ->
            text ("Error: " ++ error)


userView : (Topping -> msg) -> (Topping -> msg) -> (Topping -> Int) -> List Topping -> Html msg
userView decrease increase value toppings =
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
    let
        color =
            case value of
                0 ->
                    "lightgray"

                1 ->
                    "yellow"

                2 ->
                    "orange"

                _ ->
                    "darkorange"
    in
        div
            [ style
                [ ( "display", "inline-block" )
                , ( "background-color", color )
                , ( "padding", "5px" )
                , ( "margin", "10px" )
                ]
            ]
            [ span [ style [ ( "font-weight", "bold" ) ] ] [ text topping.name ]
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
