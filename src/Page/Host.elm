module Page.Host exposing
    ( Model
    , Msg
    , fake
    , init
    , subscriptions
    , update
    , view
    )

import Browser.Navigation as Nav
import Config exposing (Config)
import Count
import Diagram
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Error exposing (Error)
import RoomId exposing (RoomId)
import Route
import Socket
import Topping exposing (BaseTopping, Topping)
import ToppingTriplet exposing (Triplet)
import User exposing (User)
import ViewStuff exposing (guestUserView, redLink, title)



-- TYPES


type alias Model =
    { config : Config
    , roomId : RoomId
    , userCounts : Dict String Topping.Count
    , hostCount : Topping.Count
    , users : List User
    , error : Maybe Error
    }


type Msg
    = AddSliceCount User Topping Int
    | AddHostSliceCount Topping Int
    | UpdateGuestTripletFromSocket (Result Error Triplet)
    | GuestJoined (Result Error User)
    | GuestLeft (Result Error User)
    | KickOut User



-- INIT


init : RoomId -> ( Model, Cmd Msg )
init roomId =
    ( { config =
            { slices = { slicesPerPart = 2, partsPerPie = 4 }
            , toppings = { base = Topping.all, maxToppingsPerSlice = 1 }
            }
      , userCounts = Dict.empty
      , hostCount = Topping.emptyCount
      , users = []
      , roomId = roomId
      , error = Nothing
      }
    , Cmd.none
    )


fake : RoomId -> ( Model, Cmd Msg )
fake roomId =
    let
        ( model, cmd ) =
            init roomId
    in
    ( { model
        | users = [ User "Fake" ]
        , hostCount =
            Topping.all
                |> List.take 5
                |> List.map Topping.fromBase
                |> List.indexedMap (\i topping -> ( topping, 1 + i // 2 ))
                |> Topping.countFromList
      }
    , cmd
    )



-- UPDATE


update : Nav.Key -> Msg -> Model -> ( Model, Cmd Msg )
update _ msg model =
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
            , Socket.updateTriplet
                { user = user
                , topping = topping
                , count = newValue
                }
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

        UpdateGuestTripletFromSocket (Ok { user, topping, count }) ->
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

        UpdateGuestTripletFromSocket (Err err) ->
            ( { model | error = Just err }, Cmd.none )

        GuestJoined (Ok user) ->
            ( { model | users = user :: model.users }, Cmd.none )

        GuestJoined (Err err) ->
            ( { model | error = Just err }, Cmd.none )

        GuestLeft (Ok user) ->
            ( removeGuest user model, Cmd.none )

        GuestLeft (Err err) ->
            ( { model | error = Just err }, Cmd.none )

        KickOut user ->
            ( removeGuest user model, Socket.kickOut user )


removeGuest : User -> Model -> Model
removeGuest user model =
    { model
        | users = List.filter ((/=) user) model.users
        , userCounts = Dict.remove user.name model.userCounts
    }



-- VIEW


view : Model -> Element Msg
view model =
    column
        [ width fill ]
        [ row
            [ width fill
            , Background.color (rgb 1 0.7 0.5)
            , padding 30
            ]
            [ title "pizza party"
            , text <| "party id: " ++ RoomId.toString model.roomId
            ]
        , column [ padding 50, spacing 30 ]
            [ model.userCounts
                |> Dict.values
                |> (::) model.hostCount
                |> Topping.concatCounts
                |> Diagram.pies 100 model.config.slices
                |> List.map Element.html
                |> wrappedRow []
            , guestUserView AddHostSliceCount model.hostCount model.config.toppings.base
                |> el []
            , if List.isEmpty model.users then
                column []
                    [ text "But nobody came."
                    , paragraph []
                        [ text "(Tell guests to enter their order on "
                        , link [ Font.underline, Font.color (rgb 0 0 1) ]
                            { url = Route.toString (Route.Guest model.roomId)
                            , label = text (Route.roomUrl model.roomId)
                            }
                        , text ")"
                        ]
                    ]

              else
                guestsView model.users model.config.toppings.base model.userCounts
            ]
        ]


guestsView : List User -> List BaseTopping -> Dict String Topping.Count -> Element Msg
guestsView users baseToppings userCounts =
    column [ spacing 15 ]
        [ el [ Font.size 24, Font.bold ] (text "Guests")
        , users
            |> List.map (userView KickOut AddSliceCount baseToppings userCounts)
            |> column [ spacing 20 ]
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
    column [ spacing 10 ]
        [ row [ spacing 20 ]
            [ el [] (text user.name)
            , redLink (kickOut user) "kick out"
            ]
        , guestUserView (modify user) userCount baseToppings
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.error == Nothing then
        Sub.batch
            [ Socket.onGuestJoined GuestJoined
            , Socket.onTripletUpdate UpdateGuestTripletFromSocket
            , Socket.onGuestLeft GuestLeft
            ]

    else
        Sub.none
