module SvgStuff exposing
    ( angleAbove
    , arc
    , frange
    , linspace
    , middleAngle
    , mod2pi
    , pathCommand
    , polarToCartesian
    , translate
    )

import Color exposing (Color)
import Svg exposing (Svg, circle, path)
import Svg.Attributes exposing (cx, cy, d, fill, r, stroke, strokeWidth)


arc : Float -> Float -> Float -> Color -> Color -> Float -> Svg msg
arc rad startAngle endAngle fillColor strokeColor strokeWidth_ =
    if startAngle == endAngle || startAngle == endAngle - 2 * pi then
        circle
            [ cx "0"
            , cy "0"
            , r <| String.fromFloat rad
            , stroke <| Color.toCssString strokeColor
            , strokeWidth <| String.fromFloat strokeWidth_
            , fill <| Color.toCssString fillColor
            ]
            []

    else
        let
            start =
                polarToCartesian rad startAngle

            end =
                polarToCartesian rad endAngle

            arcSweep =
                if endAngle - startAngle <= degrees 180 then
                    0

                else
                    1

            pathComponents =
                [ pathCommand "M" [ start.x, start.y ]
                , pathCommand "A" [ rad, rad, 0, arcSweep, 1, end.x, end.y ]
                , pathCommand "L" [ 0, 0 ]
                , pathCommand "Z" []
                ]
        in
        path
            [ pathComponents |> String.join " " |> d
            , stroke <| Color.toCssString strokeColor
            , strokeWidth <| String.fromFloat strokeWidth_
            , fill <| Color.toCssString fillColor
            ]
            []


middleAngle : Float -> Float -> Float
middleAngle startAngle endAngle =
    if startAngle == endAngle then
        startAngle

    else if endAngle <= startAngle then
        middleAngle startAngle (endAngle + 2 * pi)

    else
        let
            middle =
                startAngle + (endAngle - startAngle) / 2
        in
        if middle == 0 || middle == pi then
            middleAngle (min startAngle endAngle) middle

        else
            middle


polarToCartesian : Float -> Float -> { x : Float, y : Float }
polarToCartesian r angle =
    { x = r * cos angle
    , y = r * sin angle
    }


pathCommand : String -> List Float -> String
pathCommand command values =
    command :: List.map String.fromFloat values |> String.join " "


mod2pi : Float -> Float
mod2pi angle =
    if angle >= 3 / 2 * pi then
        mod2pi (angle - 2 * pi)

    else if angle < -pi / 2 then
        mod2pi (angle + 2 * pi)

    else
        angle


angleAbove : Float -> Float -> Float
angleAbove start end =
    if end < start then
        angleAbove start (end + 2 * pi)

    else
        end


frange : Float -> Float -> Int -> List Float
frange start end steps =
    if start == end then
        []

    else
        List.range 0 steps
            |> List.map
                (\idx ->
                    start + (end - start) * toFloat idx / toFloat steps
                )


linspace : Float -> Float -> Float -> List Float
linspace start end diff =
    if start == end then
        [ start ]

    else
        let
            count =
                floor <| (end - start) / diff

            fakeEnd =
                start + toFloat count * diff
        in
        frange start fakeEnd count


translate : Float -> Float -> String
translate x y =
    "translate("
        ++ String.fromFloat x
        ++ ","
        ++ String.fromFloat y
        ++ ")"
