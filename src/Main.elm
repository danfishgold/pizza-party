module Main exposing (main)

import Browser exposing (application)
import Browser.Navigation
import Guest
import Host
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import RoomId
import Url exposing (Url)


type Role
    = Undetermined
    | Guest Guest.Model
    | Host Host.Model


type alias Model =
    { role : Role
    , key : Browser.Navigation.Key
    }


type Msg
    = SetRole Role
    | GuestMsg Guest.Msg
    | HostMsg Host.Msg
    | SetUrl Url
    | UrlRequest Browser.UrlRequest


init : () -> Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init () url key =
    case url.fragment of
        Nothing ->
            ( { role = Undetermined
              , key = key
              }
            , Cmd.none
            )

        Just roomIdString ->
            let
                ( guestModel, guestCmd ) =
                    Guest.initWithRoomId (RoomId.fromString roomIdString)
            in
            ( { role = Guest guestModel
              , key = key
              }
            , Cmd.map GuestMsg guestCmd
            )


fake : () -> Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
fake () url key =
    ( { role = Host Host.fake, key = key }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.role of
        Undetermined ->
            Sub.none

        Host host ->
            Host.subscriptions host |> Sub.map HostMsg

        Guest guest ->
            Guest.subscriptions guest |> Sub.map GuestMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model.role, msg ) of
        ( Undetermined, SetRole role ) ->
            ( { model | role = role }, Cmd.none )

        ( Host subModel, HostMsg subMsg ) ->
            let
                ( newRole, subCmd ) =
                    Host.update subMsg subModel
            in
            ( { model | role = Host newRole }, Cmd.map HostMsg subCmd )

        ( Guest subModel, GuestMsg subMsg ) ->
            let
                ( newRole, subCmd ) =
                    Guest.update subMsg subModel
            in
            ( { model | role = Guest newRole }, Cmd.map GuestMsg subCmd )

        _ ->
            Debug.todo "Wrong state"


view : Model -> Html Msg
view model =
    case model.role of
        Undetermined ->
            div []
                [ text "Are you the host or a guest?"
                , button [ onClick <| SetRole <| Host <| Host.initialModel ] [ text "Host" ]
                , button [ onClick <| SetRole <| Guest <| Guest.initialModel ] [ text "Guest" ]
                ]

        Host host ->
            Host.view host |> Html.map HostMsg

        Guest guest ->
            Guest.view guest |> Html.map GuestMsg


main : Program () Model Msg
main =
    application
        { init = init
        , onUrlChange = SetUrl
        , onUrlRequest = UrlRequest
        , subscriptions = subscriptions
        , update = update
        , view = \model -> { body = [ view model ], title = "Pizza Party" }
        }
