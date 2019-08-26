module Pill exposing
    ( button
    , container
    , counter
    , segmentedControl
    )

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input



-- PILL BUTTONS


type alias Colors =
    { fill : Color
    , hoverFill : Color
    , border : Color
    }


standardColors : Colors
standardColors =
    { fill = rgb 1 1 1
    , hoverFill = rgb 0.9 0.9 0.9
    , border = rgb 0 0 0
    }


type Side
    = Left
    | Right
    | Middle
    | Round


sideBorders : Side -> List (Element.Attribute msg)
sideBorders side =
    let
        specific =
            case side of
                Left ->
                    [ Border.roundEach
                        { topLeft = 500
                        , topRight = 0
                        , bottomLeft = 500
                        , bottomRight = 0
                        }
                    ]

                Right ->
                    [ Border.roundEach
                        { topRight = 500
                        , topLeft = 0
                        , bottomRight = 500
                        , bottomLeft = 0
                        }
                    ]

                Middle ->
                    [ Border.widthEach
                        { top = 2
                        , bottom = 2
                        , left = 0
                        , right = 0
                        }
                    ]

                Round ->
                    [ Border.rounded 500
                    ]
    in
    List.concat
        [ [ Border.solid
          , Border.width 2
          ]
        , specific
        ]


props : Colors -> Side -> Bool -> List (Element.Attribute msg)
props colors side withHover =
    sideBorders side
        ++ [ Border.color colors.border
           , Background.color colors.fill
           , width (shrink |> minimum 30)
           , Font.center
           ]
        ++ (if withHover then
                [ mouseOver [ Background.color colors.hoverFill ] ]

            else
                []
           )


container : List (Element.Attribute msg) -> Element msg -> Element msg
container attrs inner =
    el (props standardColors Round False ++ [ paddingXY 25 10 ] ++ attrs)
        inner


button : List (Element.Attribute msg) -> msg -> String -> Element msg
button attrs onClick content =
    Input.button
        (props standardColors Round True ++ [ width shrink, paddingXY 25 10 ] ++ attrs)
        { onPress = Just onClick, label = text content }


counter : msg -> msg -> Int -> Element.Color -> Element msg
counter decrease increase value color =
    row [ height shrink ]
        [ counterButton Left decrease "–"
        , el (props { standardColors | fill = color } Middle False)
            (text <| String.fromInt value)
        , counterButton Right increase "+"
        ]


counterButton : Side -> msg -> String -> Element msg
counterButton side onClick content =
    Input.button
        (props standardColors side True)
        { onPress = Just onClick, label = text content }


segmentedControl : ( a, List a, a ) -> a -> (a -> String) -> (a -> msg) -> Element msg
segmentedControl ( first, middle, last ) selected toString onSelect =
    let
        sidedSegments =
            List.concat
                [ [ ( first, Left ) ]
                , List.map (\seg -> ( seg, Middle )) middle
                , [ ( last, Right ) ]
                ]

        segment_ ( value, side ) =
            segment side (onSelect value) (selected == value) (toString value)
    in
    row []
        (List.map segment_ sidedSegments)


segment : Side -> msg -> Bool -> String -> Element msg
segment side onSelect isSelected content =
    Input.button
        (List.concat
            [ props standardColors side (not isSelected)
            , [ paddingXY 20 10
              , centerX
              , centerY
              ]
            , if isSelected then
                [ Background.color (rgb 0.8 0.8 0.8) ]

              else
                []
            ]
        )
        { onPress =
            if isSelected then
                Just onSelect

            else
                Nothing
        , label = text content
        }
