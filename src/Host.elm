port module Host exposing
    ( Model
    , Msg
    , fake
    , initialModel
    , subscriptions
    , update
    , view
    )

import Config exposing (Config)
import Count
import Diagram
import Dict exposing (Dict)
import Guest exposing (userView)
import Html exposing (Html, a, button, div, h1, h2, p, span, text)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)
import RoomId exposing (RoomId)
import Socket
import Stage exposing (Stage(..))
import Topping exposing (Topping)
import User exposing (User)


type alias Model =
    { config : Config
    , userCounts : Dict String Topping.Count
    , hostCount : Topping.Count
    , toppings : List Topping
    , users : List User
    , room : Stage () RoomId
    }


type Msg
    = StartRoom
    | AddSliceCount User Topping Int
    | AddHostSliceCount Topping Int
    | SetSliceCount User Topping Int
    | SendToppingList User
    | SetSocketRoom (Stage () RoomId)
    | GuestDisconnected User
    | KickOut User
    | Noop


initialModel : Model
initialModel =
    { config =
        { slicesPerPart = 2
        , partsPerPie = 4
        }
    , userCounts = Dict.empty
    , hostCount = Topping.emptyCount
    , toppings = Topping.all
    , users = []
    , room = Editing ()
    }


fake : Model
fake =
    { initialModel
        | users = [ User "Fake1", User "Fake2" ]
        , room = Success (RoomId.fromString "1")
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.room of
        Editing _ ->
            Sub.none

        Waiting _ ->
            Socket.createRoomResponseFromServer
                (Stage.applyResult always model.room
                    >> SetSocketRoom
                )

        Success _ ->
            Sub.batch
                [ Socket.sliceTripletsFromGuest
                    (Maybe.map (\( u, t, n ) -> SetSliceCount u t n)
                        >> Maybe.withDefault Noop
                    )
                , Socket.toppingListRequestFromGuest
                    (Result.toMaybe
                        >> Maybe.map SendToppingList
                        >> Maybe.withDefault Noop
                    )
                , Socket.guestDisconnectionsFromServer
                    (Maybe.map GuestDisconnected
                        >> Maybe.withDefault Noop
                    )
                ]

        Failure _ _ ->
            Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartRoom ->
            case model.room of
                Editing _ ->
                    ( { model | room = Waiting () }, Socket.createRoomAsHost )

                _ ->
                    ( model, Cmd.none )

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

        AddHostSliceCount topping delta ->
            ( { model
                | hostCount =
                    model.hostCount
                        |> Count.add topping delta
                        |> Tuple.first
              }
            , Cmd.none
            )

        SetSliceCount user topping count ->
            if List.member user model.users then
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

            else
                ( model, Cmd.none )

        SendToppingList user ->
            if List.member user (Debug.log "users" model.users) then
                ( model
                , Socket.sendToppingListOrErrorToGuest (Err "Name already exists")
                )

            else
                ( { model | users = user :: model.users }
                , Socket.sendToppingListOrErrorToGuest (Ok model.toppings)
                )

        SetSocketRoom room ->
            ( { model | room = room }, Cmd.none )

        GuestDisconnected user ->
            ( removeGuest user model, Cmd.none )

        KickOut user ->
            ( removeGuest user model, Socket.kickGuestAsHost user )

        Noop ->
            ( model, Cmd.none )


removeGuest : User -> Model -> Model
removeGuest user model =
    { model
        | users = List.filter ((/=) user) model.users
        , userCounts = Dict.remove user.name model.userCounts
    }



-- VIEW


view : Model -> Html Msg
view model =
    case model.room of
        Editing _ ->
            div []
                [ button [ onClick StartRoom ] [ text "Start" ]
                ]

        Waiting _ ->
            text "Setting up..."

        Success roomId ->
            div []
                [ text "room id: "
                , text <| RoomId.toString roomId
                , model.userCounts
                    |> Dict.values
                    |> (::) model.hostCount
                    |> Topping.concatCounts
                    |> Diagram.pies 100 model.config
                    |> div []
                , Guest.userView AddHostSliceCount model.hostCount model.toppings
                , if List.isEmpty model.users then
                    div []
                        [ p [] [ text "But nobody came." ]
                        , p []
                            [ text "(Tell guests to enter their order on "
                            , a [ href "https://pizzaparty.glitch.me" ] [ text "pizzaparty.glitch.me" ]
                            , text ")"
                            ]
                        ]

                  else
                    guestsView model.users model.toppings model.userCounts
                ]

        Failure _ error ->
            text ("Error: " ++ error)


guestsView : List User -> List Topping -> Dict String Topping.Count -> Html Msg
guestsView users toppings userCounts =
    div []
        [ h1 [] [ text "Guests" ]
        , users
            |> List.map (userView KickOut AddSliceCount toppings userCounts)
            |> div
                []
        ]


userView :
    (User -> msg)
    -> (User -> Topping -> Int -> msg)
    -> List Topping
    -> Dict String Topping.Count
    -> User
    -> Html msg
userView kickOut modify toppings userCounts user =
    let
        userCount =
            userCounts
                |> Dict.get user.name
                |> Maybe.withDefault Topping.emptyCount
    in
    div []
        [ h2 [] [ text user.name ]
        , button [ onClick <| kickOut user ] [ text "kick out" ]
        , Guest.userView (modify user) userCount toppings
        ]
