module PegApp exposing (main)

import PegLogic exposing (..)
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Element exposing (..)
import Collage exposing (..)
import Color exposing (..)
import Platform.Sub

-- MODEL

type alias Model =
  { board : Board
  , pegSelected : Maybe Loc
  , history : List Board
  , target : Board
  }

-- UPDATE

type Msg = Noop | Reset | Undo | PegSelect Loc | EmptySelect Loc

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case (model.pegSelected, msg) of
  (_, Noop) -> (model, Cmd.none)
  (_, Reset) -> init
  (_, Undo)  -> modelUndo model
  (_, PegSelect loc) -> ({ model | pegSelected = Just loc }, Cmd.none)
  (Nothing, EmptySelect loc) -> (model, Cmd.none)
  (Just loc1, EmptySelect loc2) -> let move = (loc1, loc2) in
    if validMove move model.board then
      modelMakeMove move model
    else
      (model, Cmd.none)

modelUndo : Model -> (Model, Cmd Msg)
modelUndo model = case model.history of
  []         -> (model, Cmd.none)
  prev::rest ->
    let
      newModel = { model
                 | board = prev
                 , pegSelected = Nothing
                 , history = rest
                 }
    in
      (newModel, Cmd.none)

modelMakeMove : Move -> Model -> (Model, Cmd Msg)
modelMakeMove move model =
    let
      modelNew =
          { model
          | board = makeMove move model.board
          , pegSelected = Nothing
          , history = model.board :: model.history
          }
    in
      if modelNew.board == model.target then init else (modelNew, Cmd.none)

-- VIEW

view : Model -> Html Msg
view model =
  let
    table = tableView model
    resetButton = button [onClick Reset] [Html.text "Reset"]
    undoButton = button [onClick Undo] [Html.text "Undo"]
    style =
      Html.Attributes.style <|
        [ ("position", "fixed")
        , ("top", "50%")
        , ("left", "50%")
        , ("transform", "translate(-50%, -50%)")
        ]
  in
    div [style] [table, resetButton, undoButton]

tableView : Model -> Html Msg
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
subscriptions = always Platform.Sub.none

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
