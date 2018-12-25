module Diagram exposing (pies)

import Color exposing (Color)
import Config
import Count
import Division
import Svg exposing (Svg, g, path, svg, text, text_)
import Svg.Attributes exposing (d, fill, height, stroke, textAnchor, transform, width, x, y)
import Topping exposing (Topping)


pies : Float -> Config.Slices -> Topping.Count -> List (Svg msg)
pies radius config toppingCount =
    let
        slicesPerPie =
            config.slicesPerPart * config.partsPerPie

        separated =
            Division.makePies config toppingCount

        remainder =
            Division.makePies
                { slicesPerPart = 1
                , partsPerPie = slicesPerPie
                }
                (Count.join separated.remaining separated.leftovers)

        remainderRemaining =
            case Count.toList remainder.remaining of
                [] ->
                    []

                pairs ->
                    [ pairs ]
    in
    separated.pies
        ++ remainder.pies
        ++ remainderRemaining
        |> List.map (pie radius slicesPerPie)


pie : Float -> Int -> List Topping.Pair -> Svg msg
pie radius slicesPerPie pieCount =
    let
        slicePositionInPie ( topping, count ) ( prevs, totalSliceCount ) =
            ( ( topping, totalSliceCount + 1, count ) :: prevs
            , totalSliceCount + count
            )

        sliceView ( topping, start, count ) =
            slice radius slicesPerPie start count topping

        translation =
            "translate("
                ++ String.fromFloat (radius + 1)
                ++ ","
                ++ String.fromFloat (radius + 1)
                ++ ")"
    in
    pieCount
        |> List.sortBy (negate << Tuple.second)
        |> List.foldl slicePositionInPie ( [], 0 )
        |> Tuple.first
        |> List.map sliceView
        |> g [ transform translation ]
        |> List.singleton
        |> svg
            [ width <| String.fromFloat <| radius * 2 + 2
            , height <| String.fromFloat <| radius * 2 + 2
            , Svg.Attributes.style "margin: 10px"
            ]


slice : Float -> Int -> Int -> Int -> Topping -> Svg msg
slice radius slicesPerPie slicesStart sliceCount topping =
    if sliceCount == 0 then
        g [] []

    else
        let
            startAngle =
                degrees 360 * toFloat (slicesStart - 1) / toFloat slicesPerPie

            endAngle =
                startAngle + degrees 360 * toFloat sliceCount / toFloat slicesPerPie
        in
        g []
            [ arc radius startAngle endAngle Color.white
            , arcTitle radius startAngle endAngle (Topping.toString topping) Color.black
            ]


arc : Float -> Float -> Float -> Color -> Svg msg
arc r startAngle endAngle color =
    let
        start =
            polarToCartesian r startAngle

        end =
            polarToCartesian r endAngle

        arcSweep =
            if endAngle - startAngle <= degrees 180 then
                0

            else
                1

        pathComponents =
            if startAngle == endAngle - 2 * pi then
                [ pathCommand "M" [ start.x, start.y ]
                , pathCommand "A" [ r, r, 0, arcSweep, 1, end.x, end.y ]
                ]

            else
                [ pathCommand "M" [ start.x, start.y ]
                , pathCommand "A" [ r, r, 0, arcSweep, 1, end.x, end.y ]
                , pathCommand "L" [ 0, 0 ]
                , pathCommand "Z" []
                ]
    in
    path
        [ pathComponents |> String.join " " |> d
        , stroke "black"
        , fill <| Color.toCssString color
        ]
        []


arcTitle : Float -> Float -> Float -> String -> Color -> Svg msg
arcTitle r startAngle endAngle title color =
    let
        center =
            arcCenter r startAngle endAngle
    in
    text_
        [ x <| String.fromFloat center.x
        , y <| String.fromFloat center.y
        , textAnchor "middle"
        , fill <| Color.toCssString color
        ]
        [ text title ]


arcCenter : Float -> Float -> Float -> { x : Float, y : Float }
arcCenter r startAngle endAngle =
    if endAngle <= startAngle then
        arcCenter r startAngle (endAngle + 2 * pi)

    else
        let
            alpha =
                endAngle - startAngle

            rad =
                if alpha == 2 * pi then
                    0

                else
                    2 * r / 3 * (1 - alpha / (8 * pi))

            theta =
                startAngle + alpha / 2
        in
        polarToCartesian rad theta


pathCommand : String -> List Float -> String
pathCommand command values =
    command :: List.map String.fromFloat values |> String.join " "


polarToCartesian : Float -> Float -> { x : Float, y : Float }
polarToCartesian r angle =
    { x = r * cos angle
    , y = r * sin angle
    }
