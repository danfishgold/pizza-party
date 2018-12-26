module Guest exposing
    ( Model
    , Msg
    , initialModel
    , subscriptions
    , update
    , userView
    , view
    )

import Browser.Events
import Count
import Html exposing (Html, a, button, div, form, input, p, span, text)
import Html.Attributes exposing (disabled, href, style, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode
import RoomId exposing (RoomId)
import Socket
import Stage exposing (Stage(..))
import Topping exposing (BaseTopping, Topping)
import User exposing (User)


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


fake : Model
fake =
    { initialModel
        | state =
            RoomJoining <|
                Success
                    { roomId = RoomId.fromString "1"
                    , user = { name = "fake" }
                    , baseToppings = Topping.all
                    }
    }


joinResult : Result a a -> a
joinResult res =
    case res of
        Ok a ->
            a

        Err a ->
            a


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.state of
        RoomFinding ((Waiting roomId) as stage) ->
            Socket.roomFoundResponseFromServer
                (Stage.applyResult always stage >> RoomFinding >> SetState)

        RoomJoining ((Waiting partial) as stage) ->
            Socket.baseToppingListFromHost
                (Stage.applyResult
                    (\baseToppings { roomId, user } -> { roomId = roomId, user = user, baseToppings = baseToppings })
                    stage
                    >> RoomJoining
                    >> SetState
                )

        RoomJoining (Success group) ->
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.state ) of
        ( SetState state, _ ) ->
            let
                newState =
                    case state of
                        RoomFinding (Success roomId) ->
                            RoomJoining <| Editing <| initialPartial roomId

                        state_ ->
                            state_
            in
            ( { model | state = newState }, Cmd.none )

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
            update msg_ model

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
                update msg_ model

            else
                ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- VIEW


onSubmit : msg -> Html.Attribute msg
onSubmit msg =
    Html.Events.preventDefaultOn "submit" (Json.Decode.succeed ( msg, True ))


view : Model -> Html Msg
view model =
    case model.state of
        RoomFinding stage ->
            findingView stage

        RoomJoining stage ->
            joiningView stage model.counts


findingView : Stage RoomId RoomId -> Html Msg
findingView stage =
    case Stage.data stage of
        Stage.In roomId ->
            stageForm "Enter the room number"
                "Find Room"
                RoomId.toString
                stage
                (RoomId.fromString >> EditRoomId)
                FindRoom

        Stage.Out _ ->
            Debug.todo "Forbidden state"


joiningView : Stage PartialGroup Group -> Topping.Count -> Html Msg
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
            userView AddSliceCount counts baseToppings


stageForm : String -> String -> (input -> String) -> Stage input output -> (String -> msg) -> msg -> Html msg
stageForm prompt buttonText inputToString stage onInput_ onSubmit_ =
    case Stage.data stage of
        Stage.In inputValue ->
            form
                [ onSubmit onSubmit_ ]
                [ text prompt
                , input
                    [ onInput onInput_
                    , value <| inputToString inputValue
                    , disabled <| not (Stage.canEdit stage)
                    ]
                    []
                , button
                    [ disabled <| String.isEmpty (inputToString inputValue) || not (Stage.canSubmit stage) ]
                    [ text <|
                        if Stage.waiting stage then
                            "Fetching..."

                        else
                            buttonText
                    ]
                , Stage.error stage
                    |> Maybe.map text
                    |> Maybe.withDefault (text "")
                ]

        Stage.Out _ ->
            Debug.todo "Tried to show form on Success state"



--     form
--         [ onSubmit FindRoom ]
--         [ text "Enter your name"
--         , input
--             [ onInput EditName
--               -- , value model.user.name
--             ]
--             []
--         , text "Enter the room ID"
--         , input
--             [ onInput (RoomId.fromString >> EditRoomId)
--               -- , value (RoomId.toString model.roomId)
--             ]
--             []
--         , button
--             [--disabled (String.isEmpty model.user.name)
--             ]
--             [ text "Join" ]
--         ]
-- RoomJoining _ ->
--     div []
--         [ p [] [ text "Joining..." ]
--         , p []
--             [ text "(Make sure the host is on "
--             , a [ href "https://pizzaparty.glitch.me" ] [ text "pizzaparty.glitch.me" ]
--             , text ", otherwise this won't work.)"
--             ]
--         ]
-- DeniedAccess _ error ->
--     text ("Error: " ++ error)


userView : (Topping -> Int -> msg) -> Topping.Count -> List BaseTopping -> Html msg
userView modify count baseToppings =
    let
        counter topping =
            toppingCounter
                (modify topping -1)
                (modify topping 1)
                (Count.get topping count)
                topping
    in
    count
        |> Topping.filterZeros
        |> Topping.toSortedList baseToppings
        |> List.map counter
        |> div []


toppingCounter : msg -> msg -> Int -> Topping -> Html msg
toppingCounter decrease increase value topping =
    let
        color =
            case value of
                0 ->
                    "#eee"

                1 ->
                    "yellow"

                2 ->
                    "orange"

                _ ->
                    "darkorange"
    in
    div
        [ style "display" "inline-block"
        , style "background-color" color
        , style "padding" "5px"
        , style "margin" "10px"
        ]
        [ span [ style "font-weight" "bold" ] [ text <| Topping.toString topping ]
        , div []
            [ button
                [ if value == 0 then
                    disabled True

                  else
                    onClick <| decrease
                ]
                [ text "-" ]
            , text <| String.fromInt value
            , button [ onClick <| increase ] [ text "+" ]
            ]
        ]
