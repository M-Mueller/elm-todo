module Icons exposing (plus)

import Html exposing (Html)
import Svg
import Svg.Attributes


icon : String -> String -> Int -> Html msg
icon viewbox path size =
    Svg.svg
        [ Svg.Attributes.class "inline"
        , Svg.Attributes.fill "none"
        , Svg.Attributes.stroke "currentColor"
        , Svg.Attributes.strokeWidth "2"
        , Svg.Attributes.width (String.fromInt size ++ "px")
        , Svg.Attributes.height (String.fromInt size ++ "px")
        , Svg.Attributes.viewBox viewbox
        ]
        [ Svg.path [ Svg.Attributes.d path ] [] ]


plus : Html msg
plus =
    icon "0 0 24 24" "M12 4v16m8-8H4" 24
