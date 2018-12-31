module Background exposing (background)

import Color exposing (Color)
import Html.Attributes exposing (style)
import Svg exposing (Svg, circle, g)
import Svg.Attributes exposing (cx, cy, fill, height, opacity, r, transform, width, x, y)
import SvgStuff exposing (arc, clip, symbol, useSymbol)


background : Float -> Float -> Svg msg
background width_ height_ =
    let
        rows =
            floor (height_ / 120) + 1

        cols =
            floor (width_ / 50) + 1
    in
    Svg.svg
        [ width <| String.fromFloat width_
        , height <| String.fromFloat height_
        , style "position" "absolute"
        , style "top" "0"
        , style "left" "0"
        , style "opacity" "0.5"
        , style "z-index" "-1"
        ]
        [ g [ transform "translate(-50, -50)" ] [ pizzaGrid rows cols ] ]


pizzaSlice =
    g [ transform "translate(0, 45)" ]
        [ arc 100 (-pi / 2 - pi / 8) (-pi / 2 + pi / 8) Color.darkYellow Color.white 0
        , arc 90 (-pi / 2 - pi / 8) (-pi / 2 + pi / 8) Color.yellow Color.white 0
        , circ 8 -5 -40 Color.red
        , circ 10 10 -70 Color.red
        ]


pizzaGrid : Int -> Int -> Svg msg
pizzaGrid rows cols =
    let
        isUpsideDown i j =
            modBy 2 (i + j) == 0

        translate i j =
            "translate("
                ++ String.fromInt (j * 50)
                ++ ","
                ++ String.fromInt (i * 120)
                ++ ")"

        rotate i j =
            if isUpsideDown i j then
                ""

            else
                "rotate(180)"

        slice i j =
            g [ transform <| translate i j ++ " " ++ rotate i j ]
                [ pizzaSlice ]
    in
    List.range 1 cols
        |> List.concatMap
            (\column ->
                List.range 1 rows
                    |> List.map (\row -> slice row column)
            )
        |> g []


circ : Float -> Float -> Float -> Color -> Svg msg
circ r_ cx_ cy_ fillColor =
    circle
        [ r <| String.fromFloat r_
        , cx <| String.fromFloat cx_
        , cy <| String.fromFloat cy_
        , fill <| Color.toCssString fillColor
        ]
        []
