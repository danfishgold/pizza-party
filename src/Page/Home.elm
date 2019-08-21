module Page.Home exposing (Model, Msg, init, subscriptions, update, view)

import Browser.Navigation as Nav
import Element exposing (..)
import Element.Font as Font
import Element.Input as Input
import Error exposing (Error)
import RemoteData exposing (RemoteData(..))
import RoomId
import Route
import ViewStuff exposing (configPanel, pillButton, title)



-- TYPES


type alias Model =
    { roomIdString : String
    , error : Maybe Error
    }


type Msg
    = EditRoomId String
    | JoinExistingRoom
    | CreateNewRoom



-- INIT


init : ( Model, Cmd Msg )
init =
    ( { roomIdString = ""
      , error = Nothing
      }
    , Cmd.none
    )



-- UPDATE


update : Nav.Key -> Msg -> Model -> ( Model, Cmd Msg )
update key msg model =
    case msg of
        EditRoomId roomIdString ->
            ( { model | roomIdString = roomIdString }, Cmd.none )

        JoinExistingRoom ->
            case RoomId.fromString model.roomIdString of
                Err err ->
                    ( { model | error = Just err }, Cmd.none )

                Ok roomId ->
                    ( model, Route.push key (Route.Join roomId) )

        CreateNewRoom ->
            ( model, Route.push key Route.Create )



-- VIEW


view : Model -> Element Msg
view model =
    [ el [ centerX ] (title "pizza party")
    , column [ width shrink, centerX, spacing 20 ]
        [ el [ width fill ] <| pillButton CreateNewRoom "create a new party"
        , el [ width fill ] <| pillButton JoinExistingRoom "join an existing party"
        , Input.text [] { onChange = EditRoomId, label = Input.labelAbove [] (text "room id"), placeholder = Nothing, text = model.roomIdString }
        ]
    , paragraph [ Font.size 14, width (shrink |> maximum 500) ]
        [ text "if you're not sure what to do, click the first button. "
        , text "if you're not sure what to do but someone told you to go to this website and enter a couple of digits, click the second one."
        ]
    ]
        |> column
            [ spacing 50
            ]
        |> configPanel



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
