module Page.Home exposing (Model, Msg, init, subscriptions, update, view)

import Browser.Navigation as Nav
import Element exposing (..)
import Element.Font as Font
import Element.Input as Input
import Error exposing (Error)
import Pill
import RemoteData exposing (RemoteData(..))
import RoomId exposing (RoomId)
import Route
import Size exposing (Size)
import Socket
import ViewStuff exposing (blueLink, configPanel, subtitle, title)



-- TYPES


type alias Model =
    { roomIdString : String
    , submission : RemoteData Error RoomId
    }


type Msg
    = EditRoomId String
    | JoinExistingRoom
    | RoomSearchResult (Result Error RoomId)
    | CreateNewRoom



-- INIT


init : ( Model, Cmd Msg )
init =
    ( { roomIdString = ""
      , submission = NotAsked
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
                    ( { model | submission = Failure err }, Cmd.none )

                Ok roomId ->
                    ( { model | submission = Loading }, Socket.findRoom roomId )

        RoomSearchResult (Ok roomId) ->
            ( { model | submission = Success roomId }, Route.push key (Route.Join roomId) )

        RoomSearchResult (Err error) ->
            ( { model | submission = Failure error }, Cmd.none )

        CreateNewRoom ->
            ( model, Route.push key Route.Create )



-- VIEW


view : Size -> Model -> Element Msg
view size model =
    column
        [ spacing 50
        , height fill
        , width fill
        ]
        [ title "pizza party"
        , column [ width fill, spacing 30 ]
            [ Pill.button [ width fill, height (px 60) ]
                CreateNewRoom
                "I'm hosting a party"
            , Pill.container [ width fill, height (px 60) ] <|
                row [ spacing 20, width fill, height shrink, centerY ]
                    [ text "I'm a guest"
                    , Input.text [ width fill, padding 5, centerY ]
                        { onChange = EditRoomId
                        , label = Input.labelHidden "party id"
                        , placeholder = Just (Input.placeholder [ Font.alignLeft ] (text "party id"))
                        , text = model.roomIdString
                        }
                    , blueLink JoinExistingRoom "join"
                    ]
            ]
        , paragraph [ Font.size 14, width (shrink |> maximum 500) ]
            [ text "if you're not sure what to do, click the first button. "
            , text "if you're not sure what to do but someone told you to go "
            , text "to this website and enter a couple of digits, "
            , text "do that in that input field there and then hit \"join\"."
            ]

        -- [ column [ width fill, spacing 10 ]
        --     [ subtitle "join a party"
        --     , wrappedRow [ width fill, spacing 20 ]
        --         [ Input.text [ width (fill |> minimum 100 |> maximum 150) ]
        --             { onChange = EditRoomId
        --             , label = Input.labelHidden "party id"
        --             , placeholder = Just (Input.placeholder [] (text "party id"))
        --             , text = model.roomIdString
        --             }
        --         , pillButton [] JoinExistingRoom "join"
        --         ]
        --     , case model.submission of
        --         Failure error ->
        --             text (Error.toString error)
        --         _ ->
        --             Element.none
        --     ]
        -- , column [ spacing 10, width fill ]
        --     [ subtitle "alternatively,"
        --     , paragraph [] [ text "and this is especially true if you have no idea what this website is," ]
        --     , el [ centerX ] <| pillButton [] CreateNewRoom "host a party"
        --     ]
        -- ]
        ]
        |> configPanel size



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.submission of
        Loading ->
            Socket.onRoomFound RoomSearchResult

        _ ->
            Sub.none
