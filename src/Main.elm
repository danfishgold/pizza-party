module Main exposing (main)

import Browser exposing (application)
import Browser.Navigation as Nav
import Element exposing (..)
import Error exposing (Error)
import Page.Create as Create
import Page.Guest as Guest
import Page.Home as Home
import Page.Host as Host
import Page.Join as Join
import RemoteData exposing (RemoteData(..))
import Route
import Size exposing (Size)
import Time
import Url exposing (Url)
import ViewStuff exposing (errorOverlay)


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



-- TYPES


type alias Model =
    { state : State
    , key : Nav.Key
    , size : Size
    , baseUrl : String
    }


type State
    = Home Home.Model
    | Create Create.Model
    | Join Join.Model
    | Guest Guest.Model
    | Host Host.Model


type Msg
    = HomeMsg Home.Msg
    | CreateMsg Create.Msg
    | JoinMsg Join.Msg
    | GuestMsg Guest.Msg
    | HostMsg Host.Msg
    | UrlChanged Url
    | LinkClicked Browser.UrlRequest
    | AnnoyingElmBug
    | Reload
    | SetSize Size


type alias Flags =
    { baseUrl : String
    }


errorMessage : Model -> Maybe Error
errorMessage model =
    case model.state of
        Home _ ->
            Nothing

        Join _ ->
            Nothing

        Guest { error } ->
            error

        Create { submission } ->
            case submission of
                Failure error ->
                    Just error

                _ ->
                    Nothing

        Host { error } ->
            error



-- INIT


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init { baseUrl } url key =
    let
        ( model, pageCmd ) =
            updateModelWithUrl url
                { state = Home (Tuple.first Home.init)
                , key = key
                , size = Size 0 0
                , baseUrl = baseUrl
                }
    in
    ( model, Cmd.batch [ pageCmd, Size.get SetSize ] )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model.state, msg ) of
        ( _, AnnoyingElmBug ) ->
            ( model, Cmd.none )

        ( _, Reload ) ->
            ( model, Route.reload )

        ( _, SetSize size ) ->
            ( { model | size = size }, Cmd.none )

        ( _, UrlChanged url ) ->
            updateModelWithUrl url model

        ( _, LinkClicked urlRequest ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        ( Home subModel, HomeMsg subMsg ) ->
            mapUpdate Home HomeMsg (Home.update model.key subMsg subModel) model

        ( _, HomeMsg _ ) ->
            ( model, Cmd.none )

        ( Create subModel, CreateMsg subMsg ) ->
            mapUpdate Create CreateMsg (Create.update model.key subMsg subModel) model

        ( _, CreateMsg _ ) ->
            ( model, Cmd.none )

        ( Join subModel, JoinMsg subMsg ) ->
            mapUpdate Join JoinMsg (Join.update model.key subMsg subModel) model

        ( _, JoinMsg _ ) ->
            ( model, Cmd.none )

        ( Host subModel, HostMsg subMsg ) ->
            mapUpdate Host HostMsg (Host.update model.key subMsg subModel) model

        ( _, HostMsg _ ) ->
            ( model, Cmd.none )

        ( Guest subModel, GuestMsg subMsg ) ->
            mapUpdate Guest GuestMsg (Guest.update model.key subMsg subModel) model

        ( _, GuestMsg _ ) ->
            ( model, Cmd.none )


mapUpdate : (subModel -> State) -> (subMsg -> Msg) -> ( subModel, Cmd subMsg ) -> Model -> ( Model, Cmd Msg )
mapUpdate toState toMsg ( newSubModel, cmd ) model =
    ( { model | state = toState newSubModel }, Cmd.map toMsg cmd )


updateModelWithUrl : Url -> Model -> ( Model, Cmd Msg )
updateModelWithUrl url model =
    case Route.parse url |> Maybe.withDefault Route.Home of
        Route.Home ->
            mapUpdate Home HomeMsg Home.init model

        Route.Create ->
            mapUpdate Create CreateMsg Create.init model

        Route.Join roomId ->
            mapUpdate Join JoinMsg (Join.init roomId) model

        Route.Guest roomId ->
            case model.state of
                Join subModel ->
                    case ( subModel.roomId == roomId, subModel.submission ) of
                        ( True, Success toppings ) ->
                            mapUpdate Guest GuestMsg (Guest.init roomId toppings subModel.user) model

                        _ ->
                            ( model, Route.replace model.key (Route.Join roomId) )

                _ ->
                    ( model, Route.replace model.key (Route.Join roomId) )

        Route.Host roomId ->
            case model.state of
                Create subModel ->
                    case subModel.submission of
                        Success roomId_ ->
                            if roomId == roomId_ then
                                mapUpdate Host HostMsg (Host.init roomId subModel.config) model

                            else
                                ( model, Route.replace model.key Route.Create )

                        _ ->
                            ( model, Route.replace model.key Route.Create )

                _ ->
                    ( model, Route.replace model.key Route.Create )

        Route.Fake roomId ->
            mapUpdate Host HostMsg (Host.fake roomId) model

        Route.NotFound ->
            ( model, Route.replace model.key Route.Home )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { body =
        if model.size == Size 0 0 then
            []

        else
            [ Element.layout
                [ case errorMessage model of
                    Nothing ->
                        inFront Element.none

                    Just err ->
                        inFront (errorOverlay model.size err Reload)
                ]
                (body model)
            ]
    , title = "Pizza Party"
    }


body : Model -> Element Msg
body model =
    case model.state of
        Home subModel ->
            Home.view model.size subModel |> Element.map HomeMsg

        Create subModel ->
            Create.view model.size subModel |> Element.map CreateMsg

        Join subModel ->
            Join.view model.size subModel |> Element.map JoinMsg

        Host host ->
            Host.view model.baseUrl model.size host |> Element.map HostMsg

        Guest guest ->
            Guest.view model.size guest |> Element.map GuestMsg



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ case model.state of
            Home subModel ->
                Sub.map HomeMsg (Home.subscriptions subModel)

            Join subModel ->
                Sub.map JoinMsg (Join.subscriptions subModel)

            Create subModel ->
                Sub.map CreateMsg (Create.subscriptions subModel)

            Host subModel ->
                Sub.map HostMsg (Host.subscriptions subModel)

            Guest subModel ->
                Sub.map GuestMsg (Guest.subscriptions subModel)

        {- Why is this here? Very good question.
           There's an issue with elm 0.19 where changing the url from a page
           that had no subs to a page that has subs doesn't
           change the subscriptions. So this timer is here so that every page
           has subscriptions that will definitely fire.
        -}
        , Time.every 1000 (always AnnoyingElmBug)
        , Size.onChange SetSize
        ]
