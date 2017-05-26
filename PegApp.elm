module PegApp exposing (main)

import PegLogic exposing (..)
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Element exposing (..)
import Collage exposing (..)
import Text exposing (..)
import Color exposing (..)
import Platform.Sub exposing (..)

-- MODEL

type alias Model = { game : Game , rules : Bool }

-- UPDATE

type Msg = NewGame | SelectPeg Loc | SelectMove Move | Undo | Reset | Rules

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NewGame         -> (model, initRandom)
    SelectPeg loc   -> ({ model | selectPeg loc model.game }, Cmd.none)
    SelectMove move -> ({ model | makeMove move model.game }, Cmd.none)
    Undo            -> ({ model | undoMove model.game }, Cmd.none)
    Reset           -> ({ model | resetGame model.game }, Cmd.none)
    Rules           -> ({ model | rules = not model.rules })

-- VIEW

view : Model -> Html Msg
view model =
  let
    gameView = viewGame model.game
    undoButton = button [onClick Undo] [Html.text "Undo"]
    resetButton = button [onClick Reset] [Html.text "Reset"]
    newGameButton = button [onclick NewGame] [Html.text "New Game"]
    rulesButton = button [onclick Rules] [Html.text "Show Rules"]
    style =
      Html.Attributes.style <|
        [ ("position", "fixed")
        , ("top", "50%")
        , ("left", "50%")
        , ("transform", "translate(-50%, -50%)")
        ]
  in
    div [style] [table, undoButton, resetButton, newGameButton, rulesButton]

view : Model -> Html Msg
tableView model =
  let cells = tableCells model.board |> setSelection model.pegSelected in
    Array.map (Array.toList >> tr []) cells |> toList |> table []

setSelection : (Maybe Loc) -> Array (Array (Html Msg)) -> Array (Array (Html Msg))
setSelection pegSelected cells = case pegSelected of
  Nothing -> cells
  Just (i, j) -> set i (set j circleSelected (fget i cells)) cells

tableCells : Board -> Array (Array (Html Msg))
tableCells (Board m) =
  Array.indexedMap rowCells m

rowCells : Int -> (Array Space) -> Array (Html Msg)
rowCells i row =
  Array.indexedMap (spaceCell i) row

spaceCell : Int -> Int -> Space -> Html Msg
spaceCell i j s = case s of
  None  -> td [] []
  Empty -> [circleEmpty] |> td [onClick (EmptySelect (i, j))]
  Peg   -> [circlePeg] |> td [onClick (PegSelect (i, j))]

circleEmpty = [circle 10 |> filled black] |> collage 40 40 |> toHtml
circlePeg = [circle 20 |> filled lightRed] |> collage 40 40 |> toHtml
circleSelected = [circle 20 |> filled darkRed] |> collage 40 40 |> toHtml

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions = always none

-- INIT

init : (Model, Cmd Msg)
init = let (start, target) = initializeGame () in
  ({ board = start
   , pegSelected = Nothing
   , history = []
   , target = target }
  , Cmd.none
  )

-- MAIN

main : Program Never Model Msg
main =
  program <|
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
