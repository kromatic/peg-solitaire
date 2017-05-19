module PegApp exposing (main)

import PegLogic exposing (..)
import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes
import Collage
import Element
import Color exposing (..)
import Array

-- MODEL

type alias Model =
  { board : Board
  , pegSelected : Maybe Loc
  }

-- UPDATE

type Msg = Noop | Reset | PegSelect Loc | EmptySelect Loc

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case (model.pegSelected, msg) of
  (_, Noop) -> model
  (_, Reset) -> init
  (_, PegSelect loc) -> { model | pegSelected = Just loc }
  (Nothing, EmptySelect loc) -> model
  (Just loc1, EmptySelect loc2) ->
    let
      model1 =
        { board = makeMove (loc1, loc2) model.board
        , pegSelected = Nothing
        }
    in
      if model1.board == target then init else model1

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
    Html.div [style] [table]

tableView : Model -> Html Msg
tableView model = let cells = tableCells model.board |> setSelection model.pegSelected in
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
spaceCell i j s -> case s of
  None  -> td [] []
  Empty -> [[circle 10 |> filled black] |> collage 20 20 |> toHtml]
           |> td [onClick (EmptySelect (i, j))]
  Peg   -> [[circle 10 |> filled red] |> collage 20 20 |> toHtml]
           |> td [onClick (PegSelect (i, j))]

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions = always (Keyboard.downs (\x -> Reset if x == 27 else Noop))

-- INIT

init : (Model, Cmd Msg)
init = ({ board = start, pegSelected = Nothing }, Cmd.none)

-- MAIN

main : Program Never Model Msg
main =
  Html.program <|
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
