module Guest exposing
    ( Model
    , Msg
    , initWithRoomId
    , initialModel
    , subscriptions
    , update
    , view
    )

import Browser.Navigation as Nav
import Count
import Element exposing (Element, column, text)
import Element.Input as Input exposing (button)
import RoomId exposing (RoomId)
import Socket
import Stage exposing (Stage(..))
import Topping exposing (BaseTopping, Topping)
import User exposing (User)
import ViewStuff exposing (guestUserView, onEnter)


type State
    = RoomFinding (Stage RoomId RoomId)
    | RoomJoining (Stage PartialGroup Group)


type alias Model =
    { state : State
    , counts : Topping.Count
    }


type alias PartialGroup =
    { roomId : RoomId
    , user : User
    }


type alias Group =
    { roomId : RoomId
    , user : User
    , baseToppings : List BaseTopping
    }


type Msg
    = EditRoomId RoomId
    | FindRoom
    | EditName String
    | JoinRoom
    | SetState State
    | AddSliceCount Topping Int
    | SetSliceCount Topping Int
    | HostDisconnected
    | KickedOut User
    | Noop


initialModel : Model
initialModel =
    { state = RoomFinding <| Editing <| RoomId.fromString ""
    , counts = Topping.emptyCount
    }


initWithRoomId : RoomId -> ( Model, Cmd Msg )
initWithRoomId roomId =
    ( { state = RoomFinding (Waiting roomId)
      , counts = Topping.emptyCount
      }
    , Socket.findRoomAsGuest roomId
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.state of
        RoomFinding ((Waiting _) as stage) ->
            Socket.roomFoundResponseFromServer
                (Stage.applyResult always stage >> RoomFinding >> SetState)

        RoomJoining ((Waiting _) as stage) ->
            Socket.baseToppingListFromHost
                (Stage.applyResult
                    (\baseToppings { roomId, user } -> { roomId = roomId, user = user, baseToppings = baseToppings })
                    stage
                    >> RoomJoining
                    >> SetState
                )

        RoomJoining (Success _) ->
            Sub.batch
                [ Socket.sliceTripletsFromGuest (onTripletUpdate model)
                , Socket.hostDisconnectionsFromServer HostDisconnected
                , Socket.kickedOutByHost (Maybe.map KickedOut >> Maybe.withDefault Noop)
                ]

        _ ->
            Sub.none


onTripletUpdate : Model -> Maybe ( User, Topping, Int ) -> Msg
onTripletUpdate model triplet =
    case ( model.state, triplet ) of
        ( RoomJoining (Success { user }), Just ( updatedUser, topping, count ) ) ->
            if user.name == updatedUser.name then
                SetSliceCount topping count

            else
                Noop

        _ ->
            Noop


editName : String -> PartialGroup -> PartialGroup
editName name group =
    { group | user = { name = name } }


initialPartial : RoomId -> PartialGroup
initialPartial roomId =
    { roomId = roomId, user = { name = "" } }


update : Nav.Key -> Msg -> Model -> ( Model, Cmd Msg )
update key msg model =
    case ( msg, model.state ) of
        ( SetState state, _ ) ->
            let
                ( newState, cmd ) =
                    case state of
                        RoomFinding (Success roomId) ->
                            ( RoomJoining <| Editing <| initialPartial roomId
                            , Cmd.none
                              --Route.push key (Route.Room roomId)
                            )

                        state_ ->
                            ( state_, Cmd.none )
            in
            ( { model | state = newState }, cmd )

        ( Noop, _ ) ->
            ( model, Cmd.none )

        ( EditRoomId roomId, RoomFinding stage ) ->
            ( { model | state = RoomFinding (Stage.update always roomId stage) }
            , Cmd.none
            )

        ( FindRoom, RoomFinding (Editing roomId) ) ->
            ( { model | state = RoomFinding (Waiting roomId) }
            , Socket.findRoomAsGuest roomId
            )

        ( EditName name, RoomJoining stage ) ->
            ( { model | state = RoomJoining (Stage.update editName name stage) }
            , Cmd.none
            )

        ( JoinRoom, RoomJoining (Editing partial) ) ->
            ( { model | state = RoomJoining (Waiting partial) }
            , Socket.requestToppingsListFromHost partial.user
            )

        ( AddSliceCount topping delta, RoomJoining (Success { user }) ) ->
            let
                ( newCounts, newValue ) =
                    model.counts |> Count.add topping delta
            in
            ( { model | counts = newCounts }
            , Socket.broadcastSliceTriplet user topping newValue
            )

        ( SetSliceCount topping newValue, RoomJoining (Success _) ) ->
            ( { model | counts = model.counts |> Count.set topping newValue }
            , Cmd.none
            )

        ( HostDisconnected, RoomJoining _ ) ->
            let
                msg_ =
                    "The host disconnected :("
                        |> Failure (RoomId.fromString "")
                        |> RoomFinding
                        |> SetState
            in
            update key msg_ model

        ( KickedOut kickedOutUser, RoomJoining stage ) ->
            let
                user =
                    case Stage.data stage of
                        Stage.In partialGroup ->
                            partialGroup.user

                        Stage.Out group ->
                            group.user
            in
            if kickedOutUser == user then
                let
                    msg_ =
                        "You were kicked out by the host"
                            |> Failure (RoomId.fromString "")
                            |> RoomFinding
                            |> SetState
                in
                update key msg_ model

            else
                ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Element Msg
view model =
    case model.state of
        RoomFinding stage ->
            findingView stage

        RoomJoining stage ->
            joiningView stage model.counts


findingView : Stage RoomId RoomId -> Element Msg
findingView stage =
    case Stage.data stage of
        Stage.In _ ->
            stageForm "Enter the room number"
                "Find Room"
                RoomId.toString
                stage
                (RoomId.fromString >> EditRoomId)
                FindRoom

        Stage.Out _ ->
            Debug.todo "Forbidden state"


joiningView : Stage PartialGroup Group -> Topping.Count -> Element Msg
joiningView stage counts =
    case Stage.data stage of
        Stage.In _ ->
            stageForm "Enter your username"
                "Join"
                (.user >> .name)
                stage
                EditName
                JoinRoom

        Stage.Out { baseToppings } ->
            guestUserView AddSliceCount counts baseToppings


stageForm : String -> String -> (input -> String) -> Stage input output -> (String -> msg) -> msg -> Element msg
stageForm prompt buttonText inputToString stage onInput_ onSubmit_ =
    case Stage.data stage of
        Stage.In inputValue ->
            column []
                [ Input.text [ onEnter onSubmit_ ]
                    -- disabled <| not (Stage.canEdit stage)
                    { onChange = onInput_
                    , text = inputToString inputValue
                    , placeholder = Nothing
                    , label = Input.labelAbove [] (text prompt)
                    }
                , button []
                    -- disabled <| String.isEmpty (inputToString inputValue) || not (Stage.canSubmit stage)
                    { onPress = Just onSubmit_
                    , label =
                        if Stage.waiting stage then
                            text "Fetching..."

                        else
                            text buttonText
                    }
                , Stage.error stage
                    |> Maybe.map text
                    |> Maybe.withDefault Element.none
                ]

        Stage.Out _ ->
            Debug.todo "Tried to show form on Success state"
