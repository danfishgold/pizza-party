module Main exposing (..)

import Html exposing (Html, program, div, button, text)
import Html.Events exposing (onClick)
import Guest
import Host


type Role
    = Undetermined
    | Guest Guest.Model
    | Host Host.Model


type alias Model =
    { role : Role }


type Msg
    = SetRole Role
    | GuestMsg Guest.Msg
    | HostMsg Host.Msg


init : ( Model, Cmd Msg )
init =
    ( { role = Undetermined }, Cmd.none )


fake : ( Model, Cmd Msg )
fake =
    ( { role = Host Host.fake }, Cmd.none )


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
            Debug.crash "Wrong state"


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


main : Program Never Model Msg
main =
    program
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
