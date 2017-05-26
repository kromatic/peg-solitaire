module PegApp exposing (main)

import PegLogic exposing (..)
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Element exposing (..)
import Collage exposing (..)
import Color exposing (..)
import Platform.Sub exposing (..)
import Random exposing (..)
import Array exposing (..)

-- MODEL

type alias Model = { game : Game , rules : Bool }

-- UPDATE

type Msg = NewGame
         | CreateGame Int
         | SelectPeg Loc
         | SelectMove Move
         | Undo
         | Reset
         | Rules

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NewGame         -> (model, initRandom)
    CreateGame n    -> ({ model | game = initGame n }, Cmd.none)
    SelectPeg loc   -> ({ model | game = selectPeg loc model.game }, Cmd.none)
    SelectMove move -> ({ model | game = makeMove move model.game }, Cmd.none)
    Undo            -> ({ model | game = undoMove model.game }, Cmd.none)
    Reset           -> ({ model | game = resetGame model.game }, Cmd.none)
    Rules           -> ({ model | rules = not model.rules }, Cmd.none)

-- VIEW

view : Model -> Html Msg
view model =
  let
    boardView = viewBoard model.game.board
    winView = if model.game.solved then won else Html.text ""
    rulesView = if model.rules then rules else Html.text ""
  in
    div [] [buttonsView, boardView, winView, rulesView]

won =
  let
    style = Html.Attributes.style [("height", "30px")]
    txt = Html.text "You won!"
  in
    div [style] [txt]

rules =
  let
    style = Html.Attributes.style [("width", "25%")]
    txt =
      Html.text <|
        """
        Welcome to Peg Solitaire! This is a single player puzzle. Pieces on the board,
        or pegs, are marked with large red circles, and smaller black circles
        indicate empty spaces. Pegs can jump over adjacent pegs into empty spots,
        thereby clearing the jumped peg. The objective of the game is to
        clear the board, except for a single peg in the spot circled black (i.e. the
        original empty space). This spot remains marked throughout the game for easy
        reference. To make a move, select a peg by clicking on it. The empty spaces
        into which the selected peg may be jumped are shown in grey. Click one of
        these in order to make your move. Good luck!
        """
  in
    div [style] [txt]

buttonsView : Html Msg
buttonsView =
  let
    newGameButton = button [onClick NewGame] [Html.text "new game"]
    undoButton = button [onClick Undo] [Html.text "undo"]
    resetButton = button [onClick Reset] [Html.text "reset"]
    rulesButton = button [onClick Rules] [Html.text "show rules"]
  in
    div [] [newGameButton, undoButton, resetButton, rulesButton]

viewBoard : Board -> Html Msg
viewBoard board =
  let
    style = Html.Attributes.style <|
      [ ("position", "fixed")
      , ("top", "50%")
      , ("left", "50%")
      , ("transform", "translate(-50%, -50%)")
      ]
    table =
      getCells board.grid |>
        showSelection board.pegSelected |>
          showMoves board.validMoves |> showTarget board |> makeTable
  in
    div [style] [table]

getCells : Grid -> Array (Array (Html Msg))
getCells grid =
  let rowMap x row = Array.indexedMap (\y s -> getCell (x, y) s) row in
    Array.indexedMap rowMap grid

getCell : Loc -> Space -> Html Msg
getCell loc s =
  case s of
    None  -> blankCell
    Empty -> emptyCell
    Peg   -> pegCell loc

blankCell : Html Msg
blankCell = td [] []

emptyCell : Html Msg
emptyCell = td [] [[circle 10 |> filled black] |> collage 40 40 |> toHtml]

pegCell : Loc -> Html Msg
pegCell loc =
  td [onClick (SelectPeg loc)] <|
    [[circle 17 |> filled darkRed] |> collage 40 40 |> toHtml]

showSelection :
  Maybe Loc ->  Array (Array (Html Msg)) -> Array (Array (Html Msg))
showSelection maybeLoc cells =
  case maybeLoc of
    Nothing     -> cells
    Just loc    -> setMatrix loc selectedCell cells

selectedCell : Html Msg
selectedCell = td [] [[circle 17 |> filled lightRed] |> collage 40 40 |> toHtml]

showMoves : List Move -> Array (Array (Html Msg)) -> Array (Array (Html Msg))
showMoves moves cells =
  case moves of
    []         -> cells
    move::rest -> showMove move cells |> showMoves rest

showMove : Move -> Array (Array (Html Msg)) -> Array (Array (Html Msg))
showMove move cells =
  let loc = Tuple.second move in
    setMatrix loc (moveCell move) cells

moveCell : Move -> Html Msg
moveCell move =
  td [onClick (SelectMove move)] <|
    [[circle 10 |> filled grey] |> collage 40 40 |> toHtml]

showTarget : Board -> Array (Array (Html Msg)) -> Array (Array (Html Msg))
showTarget board cells =
  let
    loc = board.targetLoc
    pegMaybe = board.pegSelected
    moves = board.validMoves
  in
    case findMove loc moves of
      Just move -> setMatrix loc (moveCellTarget move) cells
      Nothing   ->
        let
          markNormalTarget () =
            case getSpace loc board.grid of
              None  -> Debug.crash "showTarget: invalid location"
              Empty -> setMatrix loc emptyCellTarget cells
              Peg   -> setMatrix loc (pegCellTarget loc) cells
        in
          case pegMaybe of
            Just pegLoc ->
              if pegLoc == loc then
                setMatrix loc selectedCellTarget cells
              else
                markNormalTarget ()
            Nothing     -> markNormalTarget ()

findMove : Loc -> List Move -> Maybe Move
findMove loc moves =
  case moves of
    [] -> Nothing
    move::rest ->
      if Tuple.second move == loc then Just move else findMove loc rest

lineStyle = { defaultLine | width = 4 }

moveCellTarget move =
  td [onClick (SelectMove move)] <|
    [[circle 10 |> filled grey, circle 17 |> outlined lineStyle] |>
     collage 40 40 |> toHtml]

emptyCellTarget =
  td [] [[circle 10 |> filled black, circle 17 |> outlined lineStyle] |>
         collage 40 40 |> toHtml]

pegCellTarget loc =
  td [onClick (SelectPeg loc)] <|
    [[circle 17 |> filled darkRed, circle 17 |> outlined lineStyle] |>
     collage 40 40 |> toHtml]

selectedCellTarget =
  td [] [[circle 17 |> filled lightRed, circle 17 |> outlined lineStyle] |>
         collage 40 40 |> toHtml]

setMatrix (x, y) msg cells = set x (set y msg (fget x cells)) cells

makeTable cells =
  Array.map (\row -> toList row |> tr []) cells |> toList |> table []

-- COMMANDS

initRandom = generate (\n -> CreateGame n) (int 0 32)

-- INIT

init : (Model, Cmd Msg)
init = ({ game = initGame 16, rules = False }, initRandom)

-- MAIN

main : Program Never Model Msg
main =
  program <|
    { init = init
    , view = view
    , update = update
    , subscriptions = always none
    }
