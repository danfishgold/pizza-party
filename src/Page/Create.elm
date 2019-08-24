module Page.Create exposing (Model, Msg, configForHostModel, init, subscriptions, update, view)

import Browser.Navigation as Nav
import Buttons exposing (pillButton, pillSegmentedControl)
import Config exposing (Config)
import Element exposing (..)
import Error exposing (Error)
import RemoteData exposing (RemoteData(..))
import RoomId exposing (RoomId)
import Route
import Size exposing (Size)
import Socket
import ViewStuff exposing (configPanel)



-- TYPES


type alias Model =
    { submission : RemoteData Error RoomId
    , config : Config
    }


type Msg
    = CreateRoom
    | SetSomething Int
    | CreationResult (Result Error RoomId)



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

        SetSomething _ ->
            ( model, Cmd.none )


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


view : Size -> Model -> Element Msg
view size model =
    case model.submission of
        NotAsked ->
            column [ spacing 20 ]
                [ paragraph []
                    [ text "New Party" ]
                , pillSegmentedControl ( 2, [ 3 ], 4 ) 2 String.fromInt SetSomething
                , pillButton CreateRoom "Start"
                ]
                |> configPanel size

        Loading ->
            text "Setting up..." |> configPanel size

        Failure err ->
            text ("error: " ++ Error.toString err) |> configPanel size

        Success _ ->
            text "Cool! One sec" |> configPanel size



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
