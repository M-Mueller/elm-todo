module Icons exposing (plus, trash)

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


trash : Html msg
trash =
    icon
        "0 0 24 24"
        "M19 7l-.867 12.142A2 2 0 0116.138 21H7.862a2 2 0 01-1.995-1.858L5 7m5 4v6m4-6v6m1-10V4a1 1 0 00-1-1h-4a1 1 0 00-1 1v3M4 7h16"
        24
