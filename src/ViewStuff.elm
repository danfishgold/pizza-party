module ViewStuff exposing (guestUserView, onEnter)

import Color
import Count
import Element exposing (Element, centerX, column, el, fill, fromRgb, height, minimum, padding, rgb, row, shrink, spacing, text, width, wrappedRow)
import Element.Background as Background
import Element.Border as Border
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
        |> wrappedRow [ spacing 30 ]


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
    row [ width (shrink |> minimum 200) ]
        [ el [ width fill ] (text <| Topping.toString topping)
        , row [ height shrink ]
            [ button
                [ Border.roundEach
                    { topLeft = 500
                    , topRight = 0
                    , bottomLeft = 500
                    , bottomRight = 0
                    }
                , Border.color (rgb 0 0 0)
                , Border.solid
                , Border.width 2
                , width (shrink |> minimum 30)
                , Background.color (rgb 1 1 1)
                , Font.center
                , height fill
                ]
                { onPress = Just decrease, label = text "–" }
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
            , button
                [ Border.roundEach
                    { topRight = 500
                    , topLeft = 0
                    , bottomRight = 500
                    , bottomLeft = 0
                    }
                , Border.color (rgb 0 0 0)
                , Border.solid
                , Border.width 2
                , width (shrink |> minimum 30)
                , Background.color (rgb 1 1 1)
                , Font.center
                , height fill
                ]
                { onPress = Just increase, label = text "+" }
            ]
        ]
