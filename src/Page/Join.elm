module Page.Join exposing (Model, Msg, init, subscriptions, update, view)

import Browser.Navigation as Nav
import Config exposing (Config)
import Element exposing (..)
import Element.Input as Input
import Error exposing (Error)
import Pill
import RemoteData exposing (RemoteData(..))
import RoomId exposing (RoomId)
import Route
import Size exposing (Size)
import Socket
import User exposing (User)
import ViewStuff exposing (configPanel, onEnter, title)



-- TYPES


type alias Model =
    { roomId : RoomId
    , user : User
    , submission : RemoteData Error Config
    }


type Msg
    = EditUsername String
    | Submit
    | JoiningResult (Result Error Config)



-- INIT


init : RoomId -> ( Model, Cmd Msg )
init roomId =
    ( { roomId = roomId
      , user = { name = "" }
      , submission = NotAsked
      }
    , Cmd.none
    )



-- UPDATE


update : Nav.Key -> Msg -> Model -> ( Model, Cmd Msg )
update key msg model =
    case msg of
        EditUsername username ->
            ( { model | user = { name = username } }, Cmd.none )

        Submit ->
            if String.isEmpty model.user.name then
                ( { model | submission = Failure Error.EmptyUsername }, Cmd.none )

            else
                ( { model | submission = Loading }
                , Socket.joinRoom model.roomId model.user
                )

        JoiningResult (Ok config) ->
            ( { model | submission = Success config }, Route.push key (Route.Guest model.roomId) )

        JoiningResult (Err err) ->
            ( { model | submission = Failure err }, Cmd.none )



-- VIEW


view : Size -> Model -> Element Msg
view size model =
    let
        buttonTitle =
            if model.submission == Loading then
                "fetching..."

            else
                "join party"
    in
    column [ spacing 50, centerX, centerY ]
        [ title "join the party"
        , if model.submission == NotAsked then
            column [ width fill, spacing 10 ]
                [ Input.text [ Input.focusedOnLoad, onEnter Submit ]
                    { onChange = EditUsername
                    , text = model.user.name
                    , placeholder = Nothing
                    , label = Input.labelAbove [] (text "enter your name")
                    }
                , paragraph [ width fill ]
                    [ text "the host of the party will see this name (other guests won't) so make sure they know it's you" ]
                ]

          else
            Element.none
        , el [ centerX ] (Pill.button [] Submit buttonTitle)
        , case model.submission of
            Failure err ->
                text (Error.toString err)

            _ ->
                Element.none
        ]
        |> configPanel size



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.submission of
        NotAsked ->
            Sub.none

        Loading ->
            Socket.onRoomJoined JoiningResult

        Failure _ ->
            Sub.none

        Success _ ->
            Sub.none
