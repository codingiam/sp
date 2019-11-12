module Main exposing (..)

import Html
import Html.Attributes
import Html.Events
import List
import List.Extra
import Platform.Cmd
import Platform.Sub
import Random
import Random.List
import Browser

width : Int
width = 3

height : Int
height = 3

type alias Cell = Int
type alias Board = List Cell
type alias Moves = Int

type alias Model = { board : Board, moves : Moves }

type alias Flags = ()

type Msg = Reset | Shuffle Board | Slide Cell

defaultBoard : Board
defaultBoard =
    List.range 1 (width * height - 1) ++ [0]

shuffle : Board -> Cmd Msg
shuffle b = Random.generate Shuffle (Random.List.shuffle b)

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

model : Flags -> (Model, Cmd Msg)
model _ =
    ({ board = defaultBoard, moves = 0 }, Cmd.none)

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
                                                            (List.map row (List.Extra.groupsOf width m.board)) ] ]
                         , Html.div [ Html.Attributes.class "columns is-mobile is-centered" ]
                                    [ Html.text ("Moves: " ++ (String.fromInt m.moves)) ]
                         , Html.div [ Html.Attributes.class "columns is-mobile is-centered" ]
                                    [ Html.button [ Html.Events.onClick Reset ] [Html.text "Reset"] ] ]
            ]
        ]

controller : Msg -> Model -> (Model, Cmd Msg)
controller msg m =
    case msg of
        (Slide _) ->
            ({ m | moves = m.moves + 1 }, Cmd.none)
        Reset ->
            ({ m | moves = 0 }, shuffle m.board)
        (Shuffle b) ->
            ({ m | board = b }, Cmd.none)

subs : Model -> Sub Msg
subs _ = Sub.none

main : Program Flags Model Msg
main =
    Browser.element { init = model
                    , view = view
                    , update = controller
                    , subscriptions = subs
                    }
