module ViewStuff exposing (blueLink, configPanel, errorOverlay, guestUserView, onEnter, pillButton, redLink, title)

import Color
import Count
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input exposing (button)
import Html.Events
import Json.Decode as Decode
import Topping exposing (BaseTopping, Topping)


onEnter : msg -> Element.Attribute msg
onEnter msg =
    Element.htmlAttribute
        (Html.Events.on "keyup"
            (Decode.field "key" Decode.string
                |> Decode.andThen
                    (\key ->
                        if key == "Enter" then
                            Decode.succeed msg

                        else
                            Decode.fail "Not the enter key"
                    )
            )
        )


guestUserView : (Topping -> Int -> msg) -> Topping.Count -> List BaseTopping -> Element msg
guestUserView modify count baseToppings =
    let
        counter topping =
            toppingCounter
                (modify topping -1)
                (modify topping 1)
                (Count.get topping count)
                topping
    in
    count
        |> Topping.filterZeros
        |> Topping.toSortedList baseToppings
        |> List.map counter
        |> wrappedRow [ spacing 10, width shrink, centerX ]


toppingCounter : msg -> msg -> Int -> Topping -> Element msg
toppingCounter decrease increase value topping =
    let
        color =
            case value of
                0 ->
                    Color.gray |> Color.toRgba |> fromRgb

                1 ->
                    Color.yellow |> Color.toRgba |> fromRgb

                2 ->
                    Color.orange |> Color.toRgba |> fromRgb

                _ ->
                    Color.darkOrange |> Color.toRgba |> fromRgb
    in
    row
        [ width (shrink |> minimum 250)
        , Background.color (Element.rgb 0.9 0.9 0.9)
        , padding 10
        , Border.rounded 100
        ]
        [ el
            [ width fill
            , paddingEach { top = 0, bottom = 0, right = 0, left = 7 }
            ]
            (text <| Topping.toString topping)
        , pillCounter decrease increase value color
        ]


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


basePillButton : ButtonColors -> Maybe msg -> String -> Element msg
basePillButton colors onClick content =
    button
        [ Border.rounded 500
        , Border.color colors.border
        , Border.solid
        , Border.width 2
        , Font.center
        , paddingXY 25 10
        , width fill
        , height fill
        , Background.color colors.fill
        , mouseOver [ Background.color colors.hoverFill ]
        ]
        { onPress = onClick, label = text content }


pillButton : msg -> String -> Element msg
pillButton onClick =
    basePillButton standardColors (Just onClick)


pillCounter : msg -> msg -> Int -> Element.Color -> Element msg
pillCounter decrease increase value color =
    row [ height shrink ]
        [ pillCounterButton Left decrease "-"
        , el
            [ Border.color (rgb 0 0 0)
            , Border.solid
            , Border.widthEach
                { top = 2
                , bottom = 2
                , left = 0
                , right = 0
                }
            , width (shrink |> minimum 30)
            , Background.color color
            , Font.center
            , padding 3
            ]
            (text <| String.fromInt value)
        , pillCounterButton Right increase "+"
        ]


type Side
    = Left
    | Right


pillCounterButton : Side -> msg -> String -> Element msg
pillCounterButton side onClick content =
    button
        [ case side of
            Left ->
                Border.roundEach
                    { topLeft = 500
                    , topRight = 0
                    , bottomLeft = 500
                    , bottomRight = 0
                    }

            Right ->
                Border.roundEach
                    { topRight = 500
                    , topLeft = 0
                    , bottomRight = 500
                    , bottomLeft = 0
                    }
        , Border.color standardColors.border
        , Background.color standardColors.fill
        , mouseOver [ Background.color standardColors.hoverFill ]
        , Border.solid
        , Border.width 2
        , width (shrink |> minimum 30)
        , Font.center
        , height fill
        ]
        { onPress = Just onClick, label = text content }


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


type alias LinkColors =
    { text : Color
    , hoverFill : Color
    }


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


blueLink =
    baseLinkButton blueLinkColors


redLink =
    baseLinkButton redLinkColors


title : String -> Element msg
title content =
    paragraph
        [ Font.size 36
        , Font.extraBold
        , Font.variant Font.smallCaps
        ]
        [ text content ]


configPanel : Element msg -> Element msg
configPanel content =
    el
        [ centerX
        , centerY
        , padding 50
        , Border.solid
        , Border.color (rgb 0 0 0)
        , Border.width 2
        , width (shrink |> minimum 500)
        , height (shrink |> minimum 500)
        , Background.color (rgb 1 1 1)
        ]
        content


errorOverlay : String -> msg -> Element msg
errorOverlay error reload =
    el
        [ Background.color (rgba 0.5 0.5 0.5 0.5)
        , width fill
        , height fill
        , inFront
            (configPanel
                (column [ width fill, height fill, spaceEvenly ]
                    [ el [] (text error), el [ centerX ] (pillButton reload "reload") ]
                )
            )
        ]
        Element.none
