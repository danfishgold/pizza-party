module Host exposing
    ( Model
    , Msg
    , fake
    , initialModel
    , subscriptions
    , update
    , view
    )

import Browser.Navigation as Nav
import Config exposing (Config)
import Count
import Diagram
import Dict exposing (Dict)
import Element exposing (Element, column, el, link, paragraph, text, wrappedRow)
import Element.Input exposing (button)
import RoomId exposing (RoomId)
import Socket
import Stage exposing (Stage(..))
import Topping exposing (BaseTopping, Topping)
import User exposing (User)
import ViewStuff exposing (guestUserView)


type alias Model =
    { config : Config
    , userCounts : Dict String Topping.Count
    , hostCount : Topping.Count
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
        { slices = { slicesPerPart = 2, partsPerPie = 4 }
        , toppings = { base = Topping.all, maxToppingsPerSlice = 1 }
        }
    , userCounts = Dict.empty
    , hostCount = Topping.emptyCount
    , users = []
    , room = Editing ()
    }


fake : RoomId -> Model
fake roomId =
    { initialModel
        | users = [ User "Fake" ]
        , room = Success roomId
        , hostCount =
            Topping.all
                |> List.take 5
                |> List.map Topping.fromBase
                |> List.indexedMap (\i topping -> ( topping, 1 + i // 2 ))
                |> Topping.countFromList
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
                , Socket.baseToppingListRequestFromGuest
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


update : Nav.Key -> Msg -> Model -> ( Model, Cmd Msg )
update _ msg model =
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
            if List.member user model.users then
                ( model
                , Socket.sendBaseToppingListOrErrorToGuest (Err "Name already exists")
                )

            else
                ( { model | users = user :: model.users }
                , Socket.sendBaseToppingListOrErrorToGuest (Ok model.config.toppings.base)
                )

        SetSocketRoom room ->
            ( { model | room = room }
              -- , case room of
              --     Success roomId ->
              --         Route.push key (Route.Room roomId)
              --     _ ->
              --         Cmd.none
            , Cmd.none
            )

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


view : Model -> Element Msg
view model =
    case model.room of
        Editing _ ->
            button [] { onPress = Just StartRoom, label = text "Start" }

        Waiting _ ->
            text "Setting up..."

        Success roomId ->
            column []
                [ text <| "Room id: " ++ RoomId.toString roomId
                , model.userCounts
                    |> Dict.values
                    |> (::) model.hostCount
                    |> Topping.concatCounts
                    |> Diagram.pies 100 model.config.slices
                    |> List.map Element.html
                    |> wrappedRow []
                , guestUserView AddHostSliceCount model.hostCount model.config.toppings.base
                , if List.isEmpty model.users then
                    column []
                        [ text "But nobody came."
                        , paragraph []
                            [ text "(Tell guests to enter their order on "
                            , link [] { url = "https://pizzaparty.glitch.me", label = text "pizzaparty.glitch.me" }
                            , text ")"
                            ]
                        ]

                  else
                    guestsView model.users model.config.toppings.base model.userCounts
                ]

        Failure _ error ->
            text ("Error: " ++ error)


guestsView : List User -> List BaseTopping -> Dict String Topping.Count -> Element Msg
guestsView users baseToppings userCounts =
    column []
        [ el [] (text "Guests")
        , users
            |> List.map (userView KickOut AddSliceCount baseToppings userCounts)
            |> column []
        ]


userView :
    (User -> msg)
    -> (User -> Topping -> Int -> msg)
    -> List BaseTopping
    -> Dict String Topping.Count
    -> User
    -> Element msg
userView kickOut modify baseToppings userCounts user =
    let
        userCount =
            userCounts
                |> Dict.get user.name
                |> Maybe.withDefault Topping.emptyCount
    in
    column []
        [ el [] (text user.name)
        , button [] { onPress = Just (kickOut user), label = text "kick out" }
        , guestUserView (modify user) userCount baseToppings
        ]
