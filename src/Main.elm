module Main exposing (..)

import Html
import Html.Attributes
import Html.Events
import List
import List.Extra
import Random
import Random.List
import Browser

width : Int
width = 3

height : Int
height = 3

type alias Cell = Int
type alias Board = List Cell

type alias Model = { board : Board
                   , seed : Random.Seed }

type Msg = Slide Cell

defaultBoard : Board
defaultBoard =
    List.range 0 (width * height - 1)

shuffle : Board -> Random.Seed -> (Board, Random.Seed)
shuffle b s =
    Random.step (Random.List.shuffle b) s

cell : Cell -> Html.Html Msg
cell c =
    Html.td [] [ Html.button [ Html.Attributes.disabled (c == 0)
                             , Html.Attributes.style "width" "50px"
                             , Html.Attributes.style "height" "50px"
                             , Html.Events.onClick (Slide c) ]
                             [ Html.text (String.fromInt c) ] ]

row : List Cell -> Html.Html Msg
row cs =
    Html.tr [] (List.map cell cs)

model : Model
model =
    let
        (b, s) = shuffle defaultBoard (Random.initialSeed 0)
    in
        { board = b, seed = s }

view : Model -> Html.Html Msg
view m =
    Html.main_ [ Html.Attributes.class "section" ]
        [ Html.div [ Html.Attributes.class "container" ]
            [ Html.h1 [ Html.Attributes.class "title has-text-centered" ]
                      [ Html.text "Sliding Puzzle" ]
              , Html.div [ Html.Attributes.class "box" ]
                         [ Html.div [ Html.Attributes.class "columns is-mobile is-centered" ]
                                    [ Html.div [ Html.Attributes.class "column is-narrow" ]
                                               [ Html.table [ Html.Attributes.class "table" ]
                                                            (List.map row (List.Extra.groupsOf width m.board)) ] ] ]
            ]
        ]

controller : Msg -> Model -> Model
controller (Slide _) m =
    let
        (b, s) = shuffle m.board m.seed
    in
        { m | board = b, seed = s }

main : Program () Model Msg
main =
    Browser.sandbox { init = model
                    , view = view
                    , update = controller
                    }
