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
import Maybe.Extra

width : Int
width = 3

height : Int
height = 3

type alias Cell = Int
type alias Board = List Cell
type alias Moves = Int
type alias Times = Int

type alias Model = { board : Board, moves : Moves }

type alias Flags = ()

type Msg = ResetClick | Shuffle Times Board (Maybe Cell, List Cell) | Slide Cell

zeroCell : Cell
zeroCell = 0

shuffleTimes : Times
shuffleTimes = 21

defaultBoard : Board
defaultBoard =
    List.range 1 (width * height - 1) ++ [zeroCell]

shuffle : Times -> Board -> Cmd Msg
shuffle t b =
    Random.generate (Shuffle t b) (Random.List.choose (neighbors b zeroCell))

cell : Cell -> Html.Html Msg
cell c =
    Html.td [] [ Html.button [ Html.Attributes.disabled (c == zeroCell)
                             , Html.Attributes.style "width" "50px"
                             , Html.Attributes.style "height" "50px"
                             , Html.Events.onClick (Slide c) ]
                             [ Html.text (if c /= zeroCell then String.fromInt c else " ") ] ]

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
                                    [ Html.button [ Html.Events.onClick ResetClick ] [Html.text "Reset"] ] ]
            ]
        ]

up : Int -> Maybe Int
up i =
    let
        ni = i - width
        max = 0
    in
        if ni < max then Nothing else Just ni

down : Int -> Maybe Int
down i =
    let
        ni = i + width
        max = width * height - 1
    in
        if ni > max then Nothing else Just ni

left : Int -> Maybe Int
left i =
    let
         ni = i - 1
         max = i // width * width
    in
         if ni < max then Nothing else Just ni

right : Int -> Maybe Int
right i =
    let
        ni = i + 1
        max = (i // width + 1) * width
    in
        if ni >= max then Nothing else Just ni

neighbors : Board -> Cell -> List Cell
neighbors b c =
    case List.Extra.elemIndex c b of
        Just i -> Maybe.Extra.values (List.map (\ci -> List.Extra.getAt ci b) (Maybe.Extra.values [up i, down i, left i, right i]))
        Nothing -> []

swap : Board -> Cell -> Cell -> Board
swap b c1 c2 =
    case List.Extra.elemIndex c1 b of
        Just i1 -> case List.Extra.elemIndex c2 b of
                      Just i2 -> List.Extra.swapAt i1 i2 b
                      Nothing -> b
        Nothing -> b

solved : Board -> Bool
solved b =
    (b == defaultBoard)

move : Board -> Cell -> Maybe Board
move b c =
    let
        n = neighbors b zeroCell
        neighbor = List.member c n
    in
        if neighbor then Just (swap b c zeroCell) else Nothing

controller : Msg -> Model -> (Model, Cmd Msg)
controller msg m =
    case msg of
        (Shuffle t b (Just c, _)) ->
            let
                board = move b c
            in
                case board of
                    Nothing -> (m, Cmd.none)
                    Just nb -> ({ m | board = nb }, if t > 0 then shuffle (t - 1) nb else Cmd.none)
        Shuffle _ _ ( Nothing, _ ) ->
            (m, Cmd.none)
        (Slide c) ->
            let
                board = if (not (solved m.board)) then move m.board c else Nothing
            in
                case board of
                    Nothing -> (m, Cmd.none)
                    Just b -> ({ m | board = b, moves = m.moves + 1 }, Cmd.none)
        ResetClick ->
            ({ m | moves = 0 }, shuffle shuffleTimes m.board)

subs : Model -> Sub Msg
subs _ =
    Sub.none

main : Program Flags Model Msg
main =
    Browser.element { init = model
                    , view = view
                    , update = controller
                    , subscriptions = subs
                    }
