module Route exposing (Route(..), parse, push, reload, replace, roomUrl, toString)

import Browser.Navigation as Nav
import RoomId exposing (RoomId)
import Url exposing (Url)
import Url.Builder as Builder
import Url.Parser as Parser exposing ((</>), Parser)


type Route
    = Home
    | Create
    | Join RoomId
    | Guest RoomId
    | Host RoomId
    | Fake RoomId
    | NotFound


toString : Route -> String
toString route =
    case route of
        Home ->
            Builder.absolute [] []

        Create ->
            Builder.absolute [ "room", "new" ] []

        Join roomId ->
            Builder.absolute [ "room", RoomId.toString roomId, "join" ] []

        Guest roomId ->
            Builder.absolute [ "room", RoomId.toString roomId ] []

        Host roomId ->
            Builder.absolute [ "room", RoomId.toString roomId, "host" ] []

        Fake roomId ->
            Builder.absolute [ "fake", "room", RoomId.toString roomId ] []

        NotFound ->
            toString Home


roomUrl : RoomId -> String
roomUrl roomId =
    "pizzaparty.glitch.me" ++ toString (Guest roomId)


push : Nav.Key -> Route -> Cmd msg
push key route =
    Nav.pushUrl key (toString route)


replace : Nav.Key -> Route -> Cmd msg
replace key route =
    Nav.replaceUrl key (toString route)


parse : Url -> Maybe Route
parse url =
    Parser.parse parser url


parser : Parser (Route -> a) a
parser =
    Parser.oneOf
        [ Parser.top |> Parser.map Home
        , Parser.s "room" </> Parser.s "new" |> Parser.map Create
        , Parser.s "room" </> roomIdParser </> Parser.s "join" |> mapOrNotFound Join
        , Parser.s "room" </> roomIdParser |> mapOrNotFound Guest
        , Parser.s "room" </> roomIdParser </> Parser.s "host" |> mapOrNotFound Host
        , Parser.s "fake" </> Parser.s "room" </> roomIdParser |> mapOrNotFound Fake
        ]


roomIdParser : Parser (Maybe RoomId -> a) a
roomIdParser =
    Parser.string
        |> Parser.map RoomId.fromString
        |> Parser.map Result.toMaybe


mapOrNotFound : (RoomId -> Route) -> Parser (Maybe RoomId -> Route) a -> Parser (a -> b) b
mapOrNotFound toRoute =
    Parser.map (Maybe.map toRoute >> Maybe.withDefault NotFound)


reload : Cmd msg
reload =
    Nav.reload
