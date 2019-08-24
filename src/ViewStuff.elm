module ViewStuff exposing
    ( configPanel
    , errorOverlay
    , guestUserView
    , onEnter
    , subtitle
    , title
    )

import Buttons exposing (pillButton, pillCounter)
import Color
import Count
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Error exposing (Error)
import Html.Events
import Json.Decode as Decode
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


title : String -> Element msg
title content =
    paragraph
        [ Font.size 36
        , Font.extraBold
        , Font.variant Font.smallCaps
        , Font.center
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
        [ Background.color (rgba 0.5 0.5 0.5 0.5)
        , width fill
        , height fill
        , inFront
            (configPanel size
                (column [ width fill, height fill, spaceEvenly ]
                    [ el [] (text (Error.toString error)), el [ centerX ] (pillButton reload "reload") ]
                )
            )
        ]
        Element.none
