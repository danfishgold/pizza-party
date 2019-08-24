module Buttons exposing
    ( blueLink
    , pillButton
    , pillCounter
    , pillSegmentedControl
    , redLink
    )

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input exposing (button)



-- PILL BUTTONS


type alias ButtonColors =
    { fill : Color
    , hoverFill : Color
    , border : Color
    }


standardColors : ButtonColors
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


pillProps : ButtonColors -> Side -> Bool -> List (Element.Attribute msg)
pillProps colors side withHover =
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


pillButton : msg -> String -> Element msg
pillButton onClick content =
    button
        (pillProps standardColors Round True ++ [ width shrink, paddingXY 25 10 ])
        { onPress = Just onClick, label = text content }


pillCounter : msg -> msg -> Int -> Element.Color -> Element msg
pillCounter decrease increase value color =
    row [ height shrink ]
        [ pillCounterButton Left decrease "-"
        , el (pillProps { standardColors | fill = color } Middle False)
            (text <| String.fromInt value)
        , pillCounterButton Right increase "+"
        ]


pillCounterButton : Side -> msg -> String -> Element msg
pillCounterButton side onClick content =
    button
        (pillProps standardColors side True)
        { onPress = Just onClick, label = text content }


pillSegmentedControl : ( a, List a, a ) -> a -> (a -> String) -> (a -> msg) -> Element msg
pillSegmentedControl ( first, middle, last ) selected toString onSelect =
    let
        sidedSegments =
            List.concat
                [ [ ( first, Left ) ]
                , List.map (\seg -> ( seg, Middle )) middle
                , [ ( last, Right ) ]
                ]

        segment ( value, side ) =
            pillSegment side (onSelect value) (selected == value) (toString value)
    in
    row []
        (List.map segment sidedSegments)


pillSegment : Side -> msg -> Bool -> String -> Element msg
pillSegment side onSelect isSelected content =
    button
        (List.concat
            [ pillProps standardColors side (not isSelected)
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



-- LINK BUTTONS


type alias LinkColors =
    { text : Color
    , hoverFill : Color
    }


baseLinkButton : LinkColors -> msg -> String -> Element msg
baseLinkButton colors onClick content =
    el
        [ Events.onClick onClick
        , Font.underline
        , Font.color colors.text
        , padding 5
        , mouseOver [ Background.color colors.hoverFill ]
        , pointer
        ]
        (text content)


blueLinkColors : LinkColors
blueLinkColors =
    { text = rgb 0 0 1
    , hoverFill = rgb 0.9 0.9 1
    }


redLinkColors : LinkColors
redLinkColors =
    { text = rgb 1 0 0
    , hoverFill = rgb 1 0.9 0.9
    }


blueLink : msg -> String -> Element msg
blueLink =
    baseLinkButton blueLinkColors


redLink : msg -> String -> Element msg
redLink =
    baseLinkButton redLinkColors
