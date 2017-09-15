module PizzaView exposing (..)

import Svg exposing (Svg, svg, g, path, text_, text)
import Svg.Attributes exposing (width, height, d, transform, stroke, fill, x, y, textAnchor)
import Topping exposing (Topping)
import Dict exposing (Dict)
import Color exposing (Color)
import Color.Convert exposing (colorToCssRgb)
import ToppingCount exposing (ToppingCount)


pie : Float -> Int -> ToppingCount -> Svg msg
pie radius slicesPerPie toppingCount =
    let
        placeInPie ( toppingKey, count ) ( prevs, totalSliceCount ) =
            case Topping.fromKey toppingKey of
                Just topping ->
                    ( prevs ++ [ ( topping, totalSliceCount + 1, count ) ]
                    , totalSliceCount + count
                    )

                Nothing ->
                    ( prevs, totalSliceCount )

        sliceGroupView ( topping, start, count ) =
            slices radius slicesPerPie start count topping
    in
        toppingCount
            |> Dict.toList
            |> List.sortBy (negate << Tuple.second)
            |> List.foldl placeInPie ( [], 0 )
            |> Tuple.first
            |> List.map sliceGroupView
            |> g [ transform <| "translate(" ++ toString radius ++ "," ++ toString radius ++ ")" ]
            |> List.singleton
            |> svg
                [ width <| toString <| radius * 2
                , height <| toString <| radius * 2
                ]


slices : Float -> Int -> Int -> Int -> Topping -> Svg msg
slices radius slicesPerPie slicesStart sliceCount topping =
    if sliceCount == 0 then
        g [] []
    else
        let
            startAngle =
                degrees 360 * toFloat (slicesStart - 1) / toFloat slicesPerPie

            endAngle =
                degrees 360 * toFloat (slicesStart - 1 + sliceCount) / toFloat slicesPerPie
        in
            g []
                [ arc radius startAngle endAngle Color.white
                  -- , arc (radius * 0.9) startAngle endAngle Color.white
                , arcTitle radius startAngle endAngle topping.name Color.black
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
            , fill <| colorToCssRgb color
            ]
            []


arcTitle : Float -> Float -> Float -> String -> Color -> Svg msg
arcTitle r startAngle endAngle title color =
    let
        center =
            arcCenter r startAngle endAngle
    in
        text_
            [ x <| toString center.x
            , y <| toString center.y
            , textAnchor "middle"
            , fill <| colorToCssRgb color
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
    command :: List.map toString values |> String.join " "


polarToCartesian : Float -> Float -> { x : Float, y : Float }
polarToCartesian r angle =
    { x = r * cos angle
    , y = r * sin angle
    }


tally : (a -> comparable) -> (a -> value) -> List a -> Dict comparable (List value)
tally toKey toVal xs =
    tallyHelperDict toKey toVal xs Dict.empty


tallyHelperDict : (a -> comparable) -> (a -> value) -> List a -> Dict comparable (List value) -> Dict comparable (List value)
tallyHelperDict toKey toVal xs partial =
    case xs of
        [] ->
            partial

        hd :: tl ->
            (tallyHelperDict toKey toVal tl)
                (partial
                    |> Dict.update (toKey hd) (Maybe.withDefault [] >> (::) (toVal hd) >> Just)
                )


tallyReduce : (a -> comparable) -> (a -> value) -> List a -> Dict comparable (List value)
tallyReduce toKey toVal xs =
    List.foldl
        (\x ->
            Dict.update
                (toKey x)
                (Maybe.withDefault [] >> (::) (toVal x) >> Just)
        )
        Dict.empty
        xs
