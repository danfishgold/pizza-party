module Guest exposing (..)

import Html exposing (Html, program, div, input, button, text, span, a)
import Html.Attributes exposing (disabled, style, value, href)
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
    | AddSliceCount Topping Int
    | SetSliceCount Topping Int
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

        Joined _ ->
            Socket.sliceTripletsFromGuest (onTripletUpdate model)

        _ ->
            Sub.none


onTripletUpdate : Model -> Maybe ( User, Topping, Int ) -> Msg
onTripletUpdate model triplet =
    case triplet of
        Just ( user, topping, count ) ->
            if user.name == model.user.name then
                SetSliceCount topping count
            else
                Noop

        _ ->
            Noop


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

        AddSliceCount topping delta ->
            let
                ( newCounts, newValue ) =
                    model.counts |> Count.add topping delta
            in
                ( { model | counts = newCounts }
                , Socket.broadcastSliceTriplet model.user topping newValue
                )

        SetSliceCount topping newValue ->
            ( { model | counts = model.counts |> Count.set topping newValue }
            , Cmd.none
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
            div []
                [ div [] [ text "Joining..." ]
                , div []
                    [ text "(Make sure the host is on "
                    , a [ href "https://pizzaparty.glitch.me" ] [ text "pizzaparty.glitch.me" ]
                    , text ", otherwise this won't work.)"
                    ]
                ]

        Joined { toppings } ->
            userView AddSliceCount model.counts toppings

        Denied error ->
            text ("Error: " ++ error)


userView : (Topping -> Int -> msg) -> Topping.Count -> List Topping -> Html msg
userView modify count toppings =
    let
        counter topping =
            toppingCounter
                (modify topping -1)
                (modify topping 1)
                (Count.get topping count)
                topping
    in
        List.map counter toppings |> div []


toppingCounter : msg -> msg -> Int -> Topping -> Html msg
toppingCounter decrease increase value topping =
    let
        color =
            case value of
                0 ->
                    "#eee"

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
