module Diagram exposing (pies)

import Color exposing (Color)
import Config
import Count
import Division
import Svg exposing (Svg, g, path, svg, text, text_)
import Svg.Attributes exposing (d, fill, height, stroke, textAnchor, textDecoration, textLength, transform, width, x, y)
import Topping exposing (Topping)


type alias SliceGroup =
    { topping : Topping
    , startAngle : Float
    , endAngle : Float
    }


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
        wd =
            2 * radius + 300

        ht =
            2 * radius + 80

        translation =
            "translate("
                ++ String.fromFloat (wd / 2)
                ++ ","
                ++ String.fromFloat (ht / 2)
                ++ ")"
    in
    pieCount
        |> List.sortBy (negate << Tuple.second)
        |> sliceGroupsFromPairs slicesPerPie
        |> List.map (sliceGroupView radius)
        |> g [ transform translation ]
        |> List.singleton
        |> svg
            [ width <| String.fromFloat <| wd
            , height <| String.fromFloat <| ht
            , Svg.Attributes.style "margin: 10px"
            ]


sliceGroupsFromPairs : Int -> List Topping.Pair -> List SliceGroup
sliceGroupsFromPairs slicesPerPie pieCount =
    let
        sliceRange =
            degrees 360 / toFloat slicesPerPie

        offset =
            degrees -90

        helper startIndex counts =
            case counts of
                [] ->
                    []

                ( _, 0 ) :: tl ->
                    helper startIndex tl

                ( topping, count ) :: tl ->
                    { topping = topping
                    , startAngle = mod2pi <| offset + toFloat startIndex * sliceRange
                    , endAngle = mod2pi <| offset + toFloat (startIndex + count) * sliceRange
                    }
                        :: helper (startIndex + count) tl
    in
    helper 0 pieCount


sliceGroupView : Float -> SliceGroup -> Svg msg
sliceGroupView radius sliceGroup =
    g []
        [ arc radius sliceGroup.startAngle sliceGroup.endAngle Color.white
        , arcTitle radius sliceGroup Color.black
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
            if startAngle == endAngle || startAngle == endAngle - 2 * pi then
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


arcTitle : Float -> SliceGroup -> Color -> Svg msg
arcTitle r { startAngle, endAngle, topping } color =
    if abs (endAngle - startAngle) == 2 * pi || startAngle == endAngle then
        text_
            [ x "0"
            , y "0"
            , textAnchor "middle"
            ]
            [ text <| Topping.toString topping ]

    else
        let
            angle =
                middleAngle startAngle endAngle

            pt1 =
                polarToCartesian (2 / 3 * r) angle

            pt2 =
                polarToCartesian (4 / 3 * r) angle

            isOnRight =
                -pi / 2 <= angle && angle <= pi / 2
        in
        g []
            [ path
                [ d <|
                    String.join " " <|
                        [ pathCommand "M" [ pt1.x, pt1.y ]
                        , pathCommand "L" [ pt2.x, pt2.y ]
                        ]
                , stroke "black"
                , fill "none"
                ]
                []
            , text_
                [ x <| String.fromFloat pt2.x
                , y <| String.fromFloat (pt2.y - 1.5)
                , textAnchor <|
                    if isOnRight then
                        "start"

                    else
                        "end"
                , textDecoration "underline"
                , fill <| Color.toCssString color
                ]
                [ text <| Topping.toString topping ]
            ]


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


mod2pi angle =
    if angle >= 3 / 2 * pi then
        mod2pi (angle - 2 * pi)

    else if angle < -pi / 2 then
        mod2pi (angle + 2 * pi)

    else
        angle
