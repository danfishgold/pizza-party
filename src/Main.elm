module Main exposing (main)

import Browser exposing (application)
import Browser.Navigation as Nav
import Element exposing (..)
import Element.Font as Font
import Guest
import Host
import Route exposing (Route)
import Url exposing (Url)
import ViewStuff exposing (configPanel, pillButton, title)


type Role
    = Undetermined
    | Guest Guest.Model
    | Host Host.Model


type alias Model =
    { role : Role
    , key : Nav.Key
    }


type Msg
    = SetRole Role
    | GuestMsg Guest.Msg
    | HostMsg Host.Msg
    | UrlChanged Url
    | LinkClicked Browser.UrlRequest


type alias Flags =
    ()


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init () url key =
    modelFromRoute key (Route.parse url |> Maybe.withDefault Route.Home)


modelFromRoute : Nav.Key -> Route -> ( Model, Cmd Msg )
modelFromRoute key route =
    case route of
        Route.Home ->
            ( { role = Undetermined
              , key = key
              }
            , Cmd.none
            )

        Route.Room roomId ->
            let
                ( guestModel, guestCmd ) =
                    Guest.initWithRoomId roomId
            in
            ( { role = Guest guestModel
              , key = key
              }
            , Cmd.map GuestMsg guestCmd
            )

        Route.Fake roomId ->
            ( { role = Host (Host.fake roomId), key = key }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.role of
        Undetermined ->
            Sub.none

        Host host ->
            Sub.map HostMsg (Host.subscriptions host)

        Guest guest ->
            Sub.map GuestMsg (Guest.subscriptions guest)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model.role, msg ) of
        ( Undetermined, SetRole role ) ->
            ( { model | role = role }, Cmd.none )

        ( _, SetRole _ ) ->
            ( model, Cmd.none )

        ( Host subModel, HostMsg subMsg ) ->
            let
                ( newRole, subCmd ) =
                    Host.update model.key subMsg subModel
            in
            ( { model | role = Host newRole }, Cmd.map HostMsg subCmd )

        ( _, HostMsg _ ) ->
            ( model, Cmd.none )

        ( Guest subModel, GuestMsg subMsg ) ->
            let
                ( newRole, subCmd ) =
                    Guest.update model.key subMsg subModel
            in
            ( { model | role = Guest newRole }, Cmd.map GuestMsg subCmd )

        ( _, GuestMsg _ ) ->
            ( model, Cmd.none )

        ( _, UrlChanged _ ) ->
            ( model, Cmd.none )

        ( _, LinkClicked urlRequest ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )


body : Model -> Element Msg
body model =
    case model.role of
        Undetermined ->
            [ el [ centerX ] (title "pizza party")
            , column [ width shrink, centerX, spacing 20 ]
                [ el [ width fill ] <| pillButton (SetRole <| Host <| Host.initialModel) "create a new party"
                , el [ width fill ] <| pillButton (SetRole <| Guest <| Guest.initialModel) "join an existing party"
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

        Host host ->
            Host.view host |> Element.map HostMsg

        Guest guest ->
            Guest.view guest |> Element.map GuestMsg


view : Model -> Browser.Document Msg
view model =
    { body = [ Element.layout [] (body model) ]
    , title = "Pizza Party"
    }


main : Program Flags Model Msg
main =
    application
        { init = init
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
