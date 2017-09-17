port module Host exposing (..)

import Html exposing (Html, program, div, button, h1, h2, span, text)
import Html.Events exposing (onClick)
import Config exposing (Config)
import Guest exposing (userView)
import Preferences as Pref exposing (Preferences)
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


fake : Model
fake =
    { initialModel
        | userPrefs = Pref.empty
        , users = [ User "Fake1", User "Fake2" ]
        , socket = Joined ()
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
            if List.member user model.users then
                ( model, Socket.sendToppingListOrErrorToGuest (Err "Name already exists") )
            else
                ( { model | users = user :: model.users }
                , Socket.sendToppingListOrErrorToGuest (Ok model.toppings)
                )

        SetSocketState state ->
            let
                command =
                    case state of
                        Joining ->
                            Socket.requestHostConnectionFromServer

                        _ ->
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
                , if List.isEmpty model.users then
                    text ""
                  else
                    usersView (AddSliceCount -1) (AddSliceCount 1) Topping.all model.userPrefs model.users
                ]

        Denied error ->
            text ("Error: " ++ error)


usersView : (User -> Topping -> msg) -> (User -> Topping -> msg) -> List Topping -> Preferences -> List User -> Html msg
usersView decrease increase toppings prefs users =
    div []
        [ h1 [] [ text "Users" ]
        , users
            |> List.map (userView decrease increase toppings prefs)
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
