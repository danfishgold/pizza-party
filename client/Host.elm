port module Host exposing (..)

import Html exposing (Html, program, div, button, h1, h2, span, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Config exposing (Config)
import Guest exposing (userView)
import Preferences as Pref exposing (Preferences)
import ToppingCount
import User exposing (User)
import Topping exposing (Topping)
import PizzaView
import Socket exposing (State(..))


type alias Model =
    { config : Config
    , userPrefs : Preferences
    , toppings : List Topping
    , users : List User
    , socket : Socket.State ()
    }


type Msg
    = AddSliceCount Int User Topping
    | SetSliceCount Int User Topping
    | SendToppingList User
    | SetSocketState (Socket.State ())
    | Noop


initialModel : Model
initialModel =
    { config =
        { slicesPerPart = 2
        , partsPerPie = 4
        }
    , userPrefs = Pref.empty
    , toppings = Topping.all
    , users = []
    , socket = NotRequested
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.socket of
        NotRequested ->
            Sub.none

        Joining ->
            Socket.hostConnectionResponseFromServer SetSocketState

        Joined () ->
            Sub.batch
                [ Socket.sliceTripletsFromGuest
                    (Maybe.map (\( u, t, n ) -> SetSliceCount n u t)
                        >> Maybe.withDefault Noop
                    )
                , Socket.toppingListRequestFromGuest
                    (Result.toMaybe >> Maybe.map SendToppingList >> Maybe.withDefault (Debug.log "uh oh" Noop))
                ]

        Denied _ ->
            Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddSliceCount delta user topping ->
            let
                ( newPrefs, newValue ) =
                    model.userPrefs |> Pref.add user topping delta
            in
                ( { model | userPrefs = newPrefs }
                , Socket.broadcastSliceTriplet user topping newValue
                )

        SetSliceCount count user topping ->
            let
                newPrefs =
                    model.userPrefs |> Pref.set user topping count
            in
                ( { model | userPrefs = newPrefs }, Cmd.none )

        SendToppingList user ->
            ( { model | users = user :: model.users }
            , Socket.sendToppingListToGuest model.toppings
            )

        SetSocketState state ->
            let
                command =
                    case state of
                        NotRequested ->
                            Cmd.none

                        Joining ->
                            Socket.requestHostConnectionFromServer

                        Joined _ ->
                            Cmd.none

                        Denied _ ->
                            Cmd.none
            in
                ( { model | socket = state }, command )

        Noop ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    case model.socket of
        NotRequested ->
            div []
                [ text "not requested"
                , button [ onClick (SetSocketState Joining) ] [ text "Start" ]
                ]

        Joining ->
            text "Setting up..."

        Joined _ ->
            div []
                [ Pref.toToppingCount model.userPrefs
                    |> PizzaView.pies 100 model.config
                    |> div []
                , usersView (AddSliceCount -1) (AddSliceCount 1) Topping.all model.userPrefs model.users
                , h1 [] [ text "Changes" ]
                , Pref.toToppingCount model.userPrefs
                    |> ToppingCount.stableOptions model.config
                    |> List.map (\toppings -> div [] [ text <| toString toppings ])
                    |> div []
                ]

        Denied error ->
            text ("Error: " ++ error)


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
            , Guest.userView (decrease user) (increase user) value toppings
            ]



-- MAIN


main : Program Never Model Msg
main =
    program
        { init = ( initialModel, Socket.requestHostConnectionFromServer )
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
