module PegLogic exposing (Board, start, target, makeMove)

import Array exposing (..)

type Space = Peg | Empty | None

type alias Loc = (Int, Int)



type alias Move = (Loc, Loc)

type Board = Board (Array (Array Space))

-- standard starting and target boards (English variant)
start : Board
start =
  let
    short = fromList [None, None, Peg, Peg, Peg, None, None]
    long = initialize 7 (always Peg)
    mid = set 3 Empty long
  in
    Board <| fromList [short, short, long, mid, long, short, short]

target : Board
target =
  let
    short = fromList [None, None, Empty, Empty, Empty, None, None]
    long = initialize 7 (always Empty)
    mid = set 3 Peg long
  in
    Board <| fromList [short, short, long, mid, long, short, short]

-- make a (valid) move
makeMove : Move -> Board -> Board
makeMove move board =
  let (mid, end) = moveLocs move in
    board |> clear (Tuple.first move) |> clear mid |> putPeg end

validMoves : Loc -> Board -> List Move
validMoves loc board =
  if not (validLoc loc && hasPeg loc board) then
    []
  else
    List.filter (board |> flip validMove) <|
      [ (loc, L)
      , (loc, R)
      , (loc, U)
      , (loc, D)
      ]

validMove : Move -> Board -> Bool
validMove move board =
  let (mid, end) = moveLocs move in
    validLoc end && hasPeg mid board && isEmpty end board

moveLocs : Move -> (Loc, Loc)
moveLocs ((x, y), dir) = case dir of
  L -> ((x, y - 1), (x, y - 2))
  R -> ((x, y + 1), (x, y + 2))
  U -> ((x - 1, y), (x - 2, y))
  D -> ((x + 1, y), (x + 2, y))

fget : Int -> Array a -> a
fget i arr = case get i arr of
  Just val -> val
  Nothing  -> Debug.crash "fget: invalid index"

getSpace : Loc -> Board -> Space
getSpace (x, y) (Board m) = fget y (fget x m)

hasPeg : Loc -> Board -> Bool
hasPeg loc board = (getSpace loc board) == Peg

isEmpty : Loc -> Board -> Bool
isEmpty loc board = (getSpace loc board) == Empty

setSpace : Loc -> Space -> Board -> Board
setSpace (x, y) s (Board m) = set x (set y s (fget x m)) m |> Board

clear : Loc -> Board -> Board
clear loc board = setSpace loc Empty board

putPeg : Loc -> Board -> Board
putPeg loc board = setSpace loc Peg board

validLoc : Loc -> Bool
validLoc (x, y) = 0 <= x && x < 7 && 0 <= y && y < 7

-- check if the given board equals the target board
solved : Board -> Bool
solved = (==) target

{- A variant of the game is to start with a hole in (x, y) and end with a
   single peg in (x', y'). randomBoard will return a pair of (start, end) boards
   arranged in this way, making sure that it admits a solution
-}
randomBoard = identity
