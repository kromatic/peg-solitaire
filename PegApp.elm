module PegApp exposing (main)

import PegLogic exposing (..)
import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes
import Element exposing (..)
import Collage exposing (..)
import Color exposing (..)
import Array exposing (..)
import Keyboard exposing (..)

-- MODEL

type alias Model =
  { board : Board
  , pegSelected : Maybe Loc
  }

-- UPDATE

type Msg = Noop | Reset | PegSelect Loc | EmptySelect Loc

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case (model.pegSelected, msg) of
  (_, Noop) -> (model, Cmd.none)
  (_, Reset) -> init
  (_, PegSelect loc) -> ({ model | pegSelected = Just loc }, Cmd.none)
  (Nothing, EmptySelect loc) -> (model, Cmd.none)
  (Just loc1, EmptySelect loc2) ->
    let
      move = (loc1, loc2)
      model1 =
        if validMove move model.board then
          { board = makeMove (loc1, loc2) model.board
          , pegSelected = Nothing
          }
        else
          model
    in
      if model1.board == target then init else (model1, Cmd.none)

-- VIEW

view : Model -> Html Msg
view model =
  let
    table = tableView model
    style =
      Html.Attributes.style <|
        [ ("position", "fixed")
        , ("top", "50%")
        , ("left", "50%")
        , ("transform", "translate(-50%, -50%)")
        ]
  in
    div [style] [table]

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

circleEmpty = [circle 20 |> filled black] |> collage 40 40 |> toHtml
circlePeg = [circle 20 |> filled red] |> collage 40 40 |> toHtml
circleSelected = [circle 20 |> filled green] |> collage 40 40 |> toHtml

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions = always (downs (\x -> if x == 27 then Reset else Noop))

-- INIT

init : (Model, Cmd Msg)
init = ({ board = start, pegSelected = Nothing }, Cmd.none)

-- MAIN

main : Program Never Model Msg
main =
  program <|
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
