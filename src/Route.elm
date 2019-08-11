module Route exposing (Route(..), parse, push, toString)

import Browser.Navigation as Nav
import RoomId exposing (RoomId)
import Url exposing (Url)
import Url.Builder as Builder
import Url.Parser as Parser exposing ((</>), Parser)


type Route
    = Home
    | Room RoomId
    | Fake RoomId


toString : Route -> String
toString route =
    case route of
        Home ->
            Builder.absolute [] []

        Room roomId ->
            Builder.absolute [ "room", RoomId.toString roomId ] []

        Fake roomId ->
            Builder.absolute [ "fake", "room", RoomId.toString roomId ] []


push : Nav.Key -> Route -> Cmd msg
push key route =
    Nav.pushUrl key (toString route)


parse : Url -> Maybe Route
parse url =
    Parser.parse parser url


parser : Parser (Route -> a) a
parser =
    Parser.oneOf
        [ Parser.top |> Parser.map Home
        , Parser.s "fake" </> Parser.s "room" </> roomIdParser |> Parser.map Fake
        , Parser.s "room" </> roomIdParser |> Parser.map Room
        ]


roomIdParser : Parser (RoomId -> a) a
roomIdParser =
    Parser.int
        |> Parser.map String.fromInt
        |> Parser.map RoomId.fromString
