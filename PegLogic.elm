module PegLogic exposing
  ( Space (..)
  , Loc
  , Move
  , Grid
  , Board
  , Game
  , initGame
  , selectPeg
  , makeMove
  , undoMove
  , resetGame
  , getSpace
  , fget
  )

import Array exposing (..)

type Space = Peg | Empty | None

type alias Loc = (Int, Int)

type alias Move = (Loc, Loc)

type alias Grid = Array (Array Space)

type alias Board =
  { grid : Grid
  , pegSelected : Maybe Loc
  , validMoves : List Move
  , targetLoc : Loc
  }

type alias Game =
  { board : Board
  , target : Grid
  , solved : Bool
  , history : List Board
  , original : Board
  }

-- initalize a random game
initGame : Int -> Game
initGame n =
  let
    targetLoc = getLoc n
    grid = full |> clear targetLoc
    board =
      { grid = grid
      , pegSelected = Nothing
      , validMoves = []
      , targetLoc = targetLoc
      }
  in
    { board = board
    , target = empty |> putPeg targetLoc
    , solved = False
    , history = []
    , original = board
    }

-- board completely filled with pegs
full : Grid
full = initGrid Peg

-- board completely empty
empty : Grid
empty = initGrid Empty

-- a peg solitaire board completely filled with s (used with s = Empty or Peg)
initGrid : Space -> Grid
initGrid s =
  let
    short = fromList [None, None, s, s, s, None, None]
    long = repeat 7 s
  in
    fromList [short, short, long, long, long, short, short]

-- convert an integer into a location on the actual board
getLoc : Int -> Loc
getLoc n =
  if 0 <= n && n < 3        then (0, n + 2)
  else if 3 <= n && n < 6   then (1, n - 1)
  else if 6 <= n && n < 13  then (2, n - 6)
  else if 13 <= n && n < 19 then (3, n - 13)
  else if 20 <= n && n < 27 then (4, n - 20)
  else if 27 <= n && n < 30 then (5, n - 25)
  else if 30 <= n && n < 33 then (6, n - 28)
  else Debug.crash "getLoc: invalid int"

-- select a peg on the board, storing the list of valid moves for that peg
selectPeg : Loc -> Game -> Game
selectPeg loc game =
  let
    board = game.board
    boardNew =
      { board
      | pegSelected = Just loc
      , validMoves = getValidMoves loc game.board.grid
      }
  in
    { game | board = boardNew }

getValidMoves : Loc -> Grid -> List Move
getValidMoves (x, y) grid =
  List.filter (\move -> validMove move grid) <|
    [ ((x, y - 1), (x, y - 2))
    , ((x - 1, y), (x - 2, y))
    , ((x, y + 1), (x, y + 2))
    , ((x + 1, y), (x + 2, y))
    ]

validMove : Move -> Grid -> Bool
validMove (mid, end) grid =
  validLoc end && isEmpty end grid && hasPeg mid grid

-- make a (valid) move
makeMove : Move -> Game -> Game
makeMove (mid, end) game =
  let
    start = case game.board.pegSelected of
      Nothing  -> Debug.crash "makeMove: no peg selected"
      Just loc -> loc
    board = game.board
    gridNew = game.board.grid |> clear start |> clear mid |> putPeg end
    boardNew =
      { board
      | grid = gridNew
      , pegSelected = Nothing
      , validMoves = []
      }
  in
    { game
    | board = boardNew
    , solved = gridNew == game.target
    , history = game.board :: game.history
    }

-- helper functions to selectPeg and makeMove

fget : Int -> Array a -> a
fget i arr = case get i arr of
  Just val -> val
  Nothing  -> Debug.crash "fget: invalid index"

getSpace : Loc -> Grid -> Space
getSpace (x, y) grid = fget y (fget x grid)

hasPeg : Loc -> Grid -> Bool
hasPeg loc grid = (getSpace loc grid) == Peg

isEmpty : Loc -> Grid -> Bool
isEmpty loc grid = (getSpace loc grid) == Empty

setSpace : Loc -> Space -> Grid -> Grid
setSpace (x, y) s grid = set x (set y s (fget x grid)) grid

clear : Loc -> Grid -> Grid
clear loc grid = setSpace loc Empty grid

putPeg : Loc -> Grid -> Grid
putPeg loc grid = setSpace loc Peg grid

validLoc : Loc -> Bool
validLoc (x, y) = 0 <= x && x < 7 && 0 <= y && y < 7

-- undo move
undoMove : Game -> Game
undoMove game =
  case game.history of
    []           -> game
    (prev::rest) ->
      { game
      | board = prev
      , solved = False
      , history = rest
      }

-- reset game
resetGame : Game -> Game
resetGame game =
  { game
  | board = game.original
  , solved = False
  , history = []
  }
