module Main exposing (main)

import Browser exposing (application)
import Browser.Navigation as Nav
import Element exposing (Element, column, row, text)
import Element.Input exposing (button)
import Guest
import Host
import Route exposing (Route)
import Url exposing (Url)


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
    column []
        [ case model.role of
            Undetermined ->
                row []
                    [ text "Are you the host or a guest?"
                    , button [] { onPress = Just <| SetRole <| Host <| Host.initialModel, label = text "Host" }
                    , button [] { onPress = Just <| SetRole <| Guest <| Guest.initialModel, label = text "Guest" }
                    ]

            Host host ->
                Host.view host |> Element.map HostMsg

            Guest guest ->
                Guest.view guest |> Element.map GuestMsg
        ]


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
