module Icons exposing (check, edit, plus, save, trash, x)

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


check : Html msg
check =
    icon
        "0 0 24 24"
        "M5 13l4 4L19 7"
        24


edit : Html msg
edit =
    icon
        "0 0 24 24"
        "M11 5H6a2 2 0 00-2 2v11a2 2 0 002 2h11a2 2 0 002-2v-5m-1.414-9.414a2 2 0 112.828 2.828L11.828 15H9v-2.828l8.586-8.586z"
        24


save : Html msg
save =
    icon
        "0 0 24 24"
        "M8 7H5a2 2 0 00-2 2v9a2 2 0 002 2h14a2 2 0 002-2V9a2 2 0 00-2-2h-3m-1 4l-3 3m0 0l-3-3m3 3V4"
        24


x : Html msg
x =
    icon
        "0 0 24 24"
        "M6 18L18 6M6 6l12 12"
        24
