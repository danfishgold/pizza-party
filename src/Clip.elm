module Clip exposing (clip, clipPath)

import Svg exposing (Svg, defs)
import Svg.Attributes exposing (id)


clip : String -> List (Svg msg) -> Svg msg
clip name clipper =
    defs []
        [ Svg.clipPath [ id name ] clipper
        ]


clipPath : String -> Svg.Attribute msg
clipPath name =
    Svg.Attributes.clipPath <| "url(#" ++ name ++ ")"
