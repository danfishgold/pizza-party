module Page.Join exposing (Model, Msg, init, subscriptions, update, view)

import Browser.Navigation as Nav
import Config exposing (Config)
import Element exposing (..)
import Element.Input as Input
import Error exposing (Error)
import RemoteData exposing (RemoteData(..))
import RoomId exposing (RoomId)
import Route
import Socket
import User exposing (User)
import ViewStuff exposing (configPanel, onEnter, pillButton, title)



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


view : Model -> Element Msg
view model =
    let
        buttonTitle =
            if model.submission == Loading then
                "fetching..."

            else
                "join party"
    in
    column [ spacing 50 ]
        [ title "join the party"
        , if model.submission == NotAsked then
            Input.text [ onEnter Submit ]
                { onChange = EditUsername
                , text = model.user.name
                , placeholder = Nothing
                , label = Input.labelAbove [] (text "choose a username")
                }

          else
            Element.none
        , pillButton Submit buttonTitle
        , case model.submission of
            Failure err ->
                text (Error.toString err)

            _ ->
                Element.none
        ]
        |> configPanel



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
