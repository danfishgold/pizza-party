module ViewStuff exposing
    ( blueLink
    , configPanel
    , errorOverlay
    , guestUserView
    , onEnter
    , redLink
    , subtitle
    , title
    )

import Color
import Count
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Error exposing (Error)
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import Pill
import Size exposing (Size)
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
        |> Topping.toList baseToppings
        |> List.map counter
        |> wrappedRow [ spacing 10, width shrink, centerX ]


toppingCounter : msg -> msg -> Int -> Topping -> Element msg
toppingCounter decrease increase value topping =
    let
        color =
            case value of
                0 ->
                    Color.lightGray |> Color.toRgba |> fromRgb

                1 ->
                    Color.lightYellow |> Color.toRgba |> fromRgb

                _ ->
                    Color.yellow |> Color.toRgba |> fromRgb
    in
    row
        [ width (shrink |> minimum 300)
        , Background.color (Element.rgb 0.9 0.9 0.9)
        , padding 10
        , Border.rounded 100
        ]
        [ el
            [ width fill
            , paddingEach { top = 0, bottom = 0, right = 0, left = 7 }
            ]
            (text <| Topping.toString topping)
        , Pill.counter decrease increase value color
        ]


title : String -> Element msg
title content =
    paragraph
        [ Font.size 36
        , Font.extraBold
        , Font.variant Font.smallCaps
        , Font.center
        , centerX
        ]
        [ text content ]


subtitle : String -> Element msg
subtitle content =
    paragraph
        [ Font.size 20
        , Font.bold
        ]
        [ text content ]


configPanel : Size -> Element msg -> Element msg
configPanel size content =
    el
        (List.concat
            [ [ centerX
              , centerY
              , padding 50
              , Background.color (rgb 1 1 1)
              ]
            , if size.width > 700 && size.height > 700 then
                [ Border.solid
                , Border.color (rgb 0 0 0)
                , Border.width 2
                , width (px 500)
                , height (shrink |> minimum 500)

                -- , Border.shadow
                --     { offset = ( 5, 5 )
                --     , size = 0
                --     , blur = 0
                --     , color = rgb 0.8 0.8 0.8
                --     }
                ]

              else
                [ width fill
                , height fill
                ]
            ]
        )
        content


errorOverlay : Size -> Error -> msg -> Element msg
errorOverlay size error reload =
    el
        [ htmlAttribute <| Html.Attributes.style "z-index" "100"
        , Background.color (rgba 0.5 0.5 0.5 0.5)
        , width fill
        , height fill
        , inFront
            (configPanel size
                (column [ width fill, height fill, spaceEvenly ]
                    [ el [] (text (Error.toString error)), el [ centerX ] (Pill.button [] reload "reload") ]
                )
            )
        ]
        Element.none



-- LINK BUTTONS


type alias LinkColors =
    { text : Color
    , hoverFill : Color
    }


baseLinkButton : LinkColors -> msg -> String -> Element msg
baseLinkButton colors onClick content =
    Input.button
        [ Events.onClick onClick
        , Font.underline
        , Font.color colors.text
        , padding 5
        , mouseOver [ Background.color colors.hoverFill ]
        , pointer
        ]
        { onPress = Just onClick
        , label =
            text content
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


blueLink : msg -> String -> Element msg
blueLink =
    baseLinkButton blueLinkColors


redLink : msg -> String -> Element msg
redLink =
    baseLinkButton redLinkColors
