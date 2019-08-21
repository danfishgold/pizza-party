module Page.Guest exposing
    ( Model
    , Msg
    , init
    , subscriptions
    , update
    , view
    )

import Browser.Navigation as Nav
import Config exposing (Config)
import Count
import Element exposing (..)
import Error exposing (Error)
import RoomId exposing (RoomId)
import Socket
import Topping exposing (Topping)
import ToppingTriplet exposing (Triplet)
import User exposing (User)
import ViewStuff exposing (guestUserView)



-- TYPES


type alias Model =
    { roomId : RoomId
    , config : Config
    , user : User
    , counts : Topping.Count
    , error : Maybe Error
    }


type Msg
    = AddSliceCount Topping Int
    | UpdateTripletFromSocket (Result Error Triplet)
    | HostDisconnected
    | KickedOut (Result Error User)



-- INIT


init : RoomId -> Config -> User -> ( Model, Cmd Msg )
init roomId config user =
    ( { roomId = roomId
      , config = config
      , user = user
      , counts = Topping.emptyCount
      , error = Nothing
      }
    , Cmd.none
    )



-- UPDATE


update : Nav.Key -> Msg -> Model -> ( Model, Cmd Msg )
update key msg model =
    case msg of
        AddSliceCount topping delta ->
            let
                ( newCounts, newValue ) =
                    model.counts |> Count.add topping delta
            in
            ( { model | counts = newCounts }
            , Socket.updateTriplet
                { user = model.user
                , topping = topping
                , count = newValue
                }
            )

        UpdateTripletFromSocket (Ok { user, topping, count }) ->
            if user == model.user then
                ( { model | counts = model.counts |> Count.set topping count }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        UpdateTripletFromSocket (Err err) ->
            ( { model | error = Just err }
            , Cmd.none
            )

        HostDisconnected ->
            ( { model | error = Just Error.HostLeft }
            , Cmd.none
            )

        KickedOut (Ok kickedOutUser) ->
            if kickedOutUser == model.user then
                ( { model | error = Just Error.YouWereKickedOut }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        KickedOut (Err err) ->
            ( { model | error = Just err }
            , Cmd.none
            )



-- VIEW


view : Model -> Element Msg
view model =
    guestUserView AddSliceCount model.counts model.config.toppings.base



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.error == Nothing then
        Sub.batch
            [ Socket.onTripletUpdate UpdateTripletFromSocket
            , Socket.onHostLeft HostDisconnected
            , Socket.onKickOut KickedOut
            ]

    else
        Sub.none
