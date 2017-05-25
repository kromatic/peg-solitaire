module PegLogic exposing ( Game
                         , initializeGame
                         , selectPeg
                         , makeMove
                         )

import Array exposing (..)

type Space = Peg | Empty | None

type alias Loc = (Int, Int)

type alias Move = (Loc, Loc)

type alias Board = Array (Array Space)

type alias Game = { board : Board
                  , pegSelected : Maybe Loc
                  , validMoves : List Move
                  , target : Loc
                  , solved : Bool
                  }

-- initalize a random game
initializeGame : Int -> Game
initializeGame n =
  let
    target = getLoc n
    board = initBoard Peg |> clear target
  in
    { board = board
    , pegSelected = Nothing
    , validMoves = []
    , target = target
    , solved = False
    }

-- board completely filled with pegs
full : Board
full = initBoard Peg

-- board completely empty
empty : Board
empty = initBoard Empty

-- a peg solitaire board completely filled with s (used with s = Empty or Peg)
initBoard : Space -> Board
initBoard s =
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

-- make a (valid) move
makeMove : Move -> Game -> Game
makeMove (mid, end) game =
  let
    start = case game.pegSelected of
      Nothing  -> Debug.crash "makeMove: no peg selected"
      Just loc -> loc
    boardNew = game.board |> clear start |> clear mid |> putPeg end
  in
    { game
    | board = boardNew
    , pegSelected = Nothing
    , validMoves = []
    , solved = solvedPuzzle boardNew game.target
    }

-- check if we have reached the target board
solvedPuzzle : Board -> Loc -> Bool
solvedPuzzle board target = board == (empty |> putPeg target)

-- select a peg on the board, creating a list of valid moves
selectPeg : Loc -> Game -> Game
selectPeg loc game =
  { game
  | pegSelected = Just loc
  , validMoves = getValidMoves loc game.board
  }

getValidMoves : Loc -> Board -> List Move
getValidMoves (x, y) board =
  List.filter (\move -> validMove move board) <|
    [ ((x, y - 1), (x, y - 2))
    , ((x - 1, y), (x - 2, y))
    , ((x, y + 1), (x, y + 2))
    , ((x + 1, y), (x + 2, y))
    ]

validMove : Move -> Board -> Bool
validMove (mid, end) board =
  validLoc end && isEmpty end board && hasPeg mid board

fget : Int -> Array a -> a
fget i arr = case get i arr of
  Just val -> val
  Nothing  -> Debug.crash "fget: invalid index"

getSpace : Loc -> Board -> Space
getSpace (x, y) board = fget y (fget x board)

hasPeg : Loc -> Board -> Bool
hasPeg loc board = (getSpace loc board) == Peg

isEmpty : Loc -> Board -> Bool
isEmpty loc board = (getSpace loc board) == Empty

setSpace : Loc -> Space -> Board -> Board
setSpace (x, y) s board = set x (set y s (fget x board)) board

clear : Loc -> Board -> Board
clear loc board = setSpace loc Empty board

putPeg : Loc -> Board -> Board
putPeg loc board = setSpace loc Peg board

validLoc : Loc -> Bool
validLoc (x, y) = 0 <= x && x < 7 && 0 <= y && y < 7
