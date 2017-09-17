port module Host exposing (..)

import Html exposing (Html, program, div, button, h1, h2, span, text)
import Html.Events exposing (onClick)
import Config exposing (Config)
import Guest exposing (userView)
import User exposing (User)
import Topping exposing (Topping)
import Diagram
import Socket exposing (State(..))
import Dict exposing (Dict)
import Count


type alias Model =
    { config : Config
    , userCounts : Dict String Topping.Count
    , toppings : List Topping
    , users : List User
    , socket : Socket.State ()
    }


type Msg
    = AddSliceCount User Topping Int
    | SetSliceCount User Topping Int
    | SendToppingList User
    | SetSocketState (Socket.State ())
    | Noop


initialModel : Model
initialModel =
    { config =
        { slicesPerPart = 2
        , partsPerPie = 4
        }
    , userCounts = Dict.empty
    , toppings = Topping.all
    , users = []
    , socket = NotRequested
    }


fake : Model
fake =
    { initialModel
        | users = [ User "Fake1", User "Fake2" ]
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
                    (Maybe.map (\( u, t, n ) -> SetSliceCount u t n)
                        >> Maybe.withDefault Noop
                    )
                , Socket.toppingListRequestFromGuest
                    (Result.toMaybe
                        >> Maybe.map SendToppingList
                        >> Maybe.withDefault (Debug.log "uh oh" Noop)
                    )
                ]

        Denied _ ->
            Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddSliceCount user topping delta ->
            let
                ( newCount, newValue ) =
                    model.userCounts
                        |> Dict.get user.name
                        |> Maybe.withDefault Topping.emptyCount
                        |> Count.add topping delta
            in
                ( { model
                    | userCounts =
                        Dict.insert user.name newCount model.userCounts
                  }
                , Socket.broadcastSliceTriplet user topping newValue
                )

        SetSliceCount user topping count ->
            let
                newCounts =
                    model.userCounts
                        |> Dict.update user.name
                            (Maybe.withDefault Topping.emptyCount
                                >> Count.set topping count
                                >> Just
                            )
            in
                ( { model | userCounts = newCounts }, Cmd.none )

        SendToppingList user ->
            if List.member user model.users then
                ( model
                , Socket.sendToppingListOrErrorToGuest (Err "Name already exists")
                )
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
                [ model.userCounts
                    |> Dict.values
                    |> Topping.concatCounts
                    |> Diagram.pies 100 model.config
                    |> div []
                , if List.isEmpty model.users then
                    text "But nobody came."
                  else
                    div []
                        [ h1 [] [ text "Guests" ]
                        , model.users
                            |> List.map (userView AddSliceCount model.toppings model.userCounts)
                            |> div
                                []
                        ]
                ]

        Denied error ->
            text ("Error: " ++ error)


userView :
    (User -> Topping -> Int -> msg)
    -> List Topping
    -> Dict String Topping.Count
    -> User
    -> Html msg
userView modify toppings userCounts user =
    let
        userCount user =
            userCounts
                |> Dict.get user.name
                |> Maybe.withDefault Topping.emptyCount
    in
        div []
            [ h2 [] [ text user.name ]
            , Guest.userView (modify user) (userCount user) toppings
            ]
