module Main exposing (..)

import Html
import Html.Attributes
import List
import List.Extra
import Random
import Random.List

width : Int
width = 3

height : Int
height = 3

board : List String
board = List.map String.fromInt (List.range 1 (width * height - 1)) ++ [" "]

shuffle : List String -> List String
shuffle list =
    let
        (l, s) = Random.step (Random.List.shuffle list) (Random.initialSeed 0)
    in
        l

rows : List String -> List (List String)
rows = List.Extra.groupsOf width

cell : String -> Html.Html msg
cell s = Html.td [] [ Html.button [ Html.Attributes.disabled (s == " ")
                                    , Html.Attributes.style "width" "50px"
                                    , Html.Attributes.style "height" "50px"] [Html.text s] ]

row : List String -> Html.Html msg
row ss = Html.tr [] (List.map cell ss)

main =
    Html.div []
        [Html.table [] (List.map row (rows (shuffle board)))]
