module Size exposing (Size, get, onChange)

import Browser.Dom as Dom
import Browser.Events
import Task


fromViewport : Dom.Viewport -> Size
fromViewport { viewport } =
    { width = floor viewport.width
    , height = floor viewport.height
    }


fromTwoInts : Int -> Int -> Size
fromTwoInts wd ht =
    { width = wd
    , height = ht
    }


get : (Size -> msg) -> Cmd msg
get toMsg =
    Dom.getViewport
        |> Task.perform fromViewport
        |> Cmd.map toMsg


onChange : (Size -> msg) -> Sub msg
onChange toMsg =
    Browser.Events.onResize fromTwoInts |> Sub.map toMsg


type alias Size =
    { width : Int
    , height : Int
    }
