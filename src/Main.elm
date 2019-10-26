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

type alias Cell = Int
type alias Board = List Cell

board : Board
board = List.range 0 (width * height - 1)

shuffle : Board -> Board
shuffle list =
    let
        (l, _) = Random.step (Random.List.shuffle list) (Random.initialSeed 0)
    in
        l

rows : Board -> List (List Cell)
rows = List.Extra.groupsOf width

cell : Cell -> Html.Html msg
cell v = Html.td [] [ Html.button [ Html.Attributes.disabled (v == 0)
                                    , Html.Attributes.style "width" "50px"
                                    , Html.Attributes.style "height" "50px"] [Html.text (String.fromInt v)] ]

row : List Cell -> Html.Html msg
row ss = Html.tr [] (List.map cell ss)

toHtml : Board -> Html.Html msg
toHtml b = Html.table [ Html.Attributes.class "table" ] (List.map row (rows b))

main =
    Html.main_ [ Html.Attributes.class "section" ]
        [ Html.div [ Html.Attributes.class "container" ]
            [ Html.h1 [ Html.Attributes.class "title has-text-centered" ]
                      [ Html.text "Sliding Puzzle" ]
              , Html.div [ Html.Attributes.class "box" ]
                         [ Html.div [ Html.Attributes.class "columns is-mobile is-centered" ]
                                    [ Html.div [ Html.Attributes.class "column is-narrow" ]
                                               [ toHtml (shuffle board) ] ] ]
            ]
        ]
