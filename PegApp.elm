module PegApp exposing (main)

import Html exposing (..)
import PegLogic exposing (..)

-- MODEL

type alias Model =
  { board : Board
  , selected : Maybe Loc
  , moves : List Move
  }

-- UPDATE

type Msg = Reset | Select Loc | Unselect | MakeMove Move

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
  Reset        -> init
  Unselect      -> { model | selected = Nothing, moves = [] }
  Select loc    -> { model
                   | selected = Just loc
                   , moves = validMoves loc model.board
                   }
  MakeMove move -> { board = makeMove move board
                   , selected = Nothing
                   , moves = []
                   }

-- VIEW

view : Model -> Html Msg

-- SUBSCRIPTIONS

-- INIT

init : (Model, Cmd Msg)
init = ({ board = start, selected = Nothing, moves = [] }, Cmd.none)
