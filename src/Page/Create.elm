module Page.Create exposing (Model, Msg, configForHostModel, init, subscriptions, update, view)

import Browser.Navigation as Nav
import Config exposing (Config)
import Element exposing (..)
import RemoteData exposing (RemoteData(..))
import RoomId exposing (RoomId)
import Route
import Socket
import ViewStuff exposing (configPanel, pillButton)



-- TYPES


type alias Model =
    { submission : RemoteData String RoomId
    , config : Config
    }


type Msg
    = CreateRoom
    | CreationResult (Result String RoomId)



-- INIT


init : ( Model, Cmd Msg )
init =
    ( { submission = NotAsked
      , config = Config.default
      }
    , Cmd.none
    )



-- UPDATE


update : Nav.Key -> Msg -> Model -> ( Model, Cmd Msg )
update key msg model =
    case msg of
        CreateRoom ->
            ( { model | submission = Loading }, Socket.createRoom model.config )

        CreationResult (Ok roomId) ->
            ( { model | submission = Success roomId }, Route.push key (Route.Host roomId) )

        CreationResult (Err err) ->
            ( { model | submission = Failure err }, Cmd.none )


configForHostModel : RoomId -> Model -> Maybe RoomId
configForHostModel roomId model =
    case model.submission of
        Success roomId_ ->
            if roomId == roomId_ then
                Just roomId_

            else
                Nothing

        _ ->
            Nothing



-- VIEW


view : Model -> Element Msg
view model =
    case model.submission of
        NotAsked ->
            column []
                [ paragraph []
                    [ text "New Party" ]
                , pillButton CreateRoom "Start"
                ]
                |> configPanel

        Loading ->
            text "Setting up..." |> configPanel

        Failure err ->
            text ("error: " ++ err) |> configPanel

        Success _ ->
            text "Cool! One sec" |> configPanel



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.submission of
        NotAsked ->
            Sub.none

        Loading ->
            Socket.onRoomCreated CreationResult

        Failure _ ->
            Sub.none

        Success _ ->
            Sub.none
