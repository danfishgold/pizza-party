module Diagram exposing (pies)

import Color exposing (Color)
import Config
import Count
import Division
import Svg exposing (Svg, g, svg, text, text_)
import Svg.Attributes exposing (dominantBaseline, fill, height, textAnchor, transform, width, x, y)
import SvgStuff as S
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
        ( wd, ht ) =
            ( 2 * radius + 300
            , 2 * radius + 80
            )
    in
    pieCount
        |> List.sortBy (negate << Tuple.second)
        |> sliceGroupsFromPairs slicesPerPie
        |> List.map (sliceGroupView radius)
        |> g [ transform <| S.translate (wd / 2) (ht / 2) ]
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
                    , startAngle = S.mod2pi <| offset + toFloat startIndex * sliceRange
                    , endAngle = S.mod2pi <| offset + toFloat (startIndex + count) * sliceRange
                    }
                        :: helper (startIndex + count) tl
    in
    helper 0 pieCount


sliceGroupView : Float -> SliceGroup -> Svg msg
sliceGroupView radius sliceGroup =
    g []
        [ S.arc radius sliceGroup.startAngle sliceGroup.endAngle Color.orange Color.white 5
        , arcTitle radius sliceGroup Color.black
        ]


arcTitle : Float -> SliceGroup -> Color -> Svg msg
arcTitle r { startAngle, endAngle, topping } color =
    let
        angle =
            S.middleAngle startAngle endAngle

        p0 =
            S.polarToCartesian (1.15 * r) angle

        anchor =
            if -pi / 2 < angle && angle < pi / 2 then
                "start"

            else if angle == -pi / 2 || angle == pi / 2 then
                "middle"

            else
                "end"

        baseline =
            if 0 < angle && angle < pi then
                "hanging"

            else if angle == 0 || angle == pi then
                "middle"

            else
                "baseline"
    in
    g []
        [ text_
            [ x <| String.fromFloat p0.x
            , y <| String.fromFloat p0.y
            , textAnchor anchor
            , dominantBaseline baseline
            , fill <| Color.toCssString color
            ]
            [ text <| Topping.toString topping ]
        ]
