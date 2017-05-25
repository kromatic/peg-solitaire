module PegLogic exposing ( Board(..)
                         , Space(..)
                         , Loc
                         , Move
                         , initializeGame
                         , makeMove
                         , validMove
                         , fget
                         )

import Array exposing (..)

type Space = Peg | Empty | None

type alias Loc = (Int, Int)

type alias Move = (Loc, Loc)

type Board = Board (Array (Array Space))

-- initalize a random game with pair of start and target boards
initializeGame : () -> (Board, Board)
initializeGame _ = (start, target)

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

-- make a move, assuming it is valid
makeMove : Move -> Board -> Board
makeMove (start, end) board =
  let mid = avgLoc start end in
    board |> clear start |> clear mid |> putPeg end

validMove : Move -> Board -> Bool
validMove (start, end) board =
  let mid = avgLoc start end in
    (  validLoc end
    && validDiff start end
    && hasPeg mid board
    && isEmpty end board
    )

avgLoc (x, y) (z, w) = ((x + z) // 2, (y + w) // 2)

validDiff (x, y) (x1, y1) =
  if x - x1 == 0 then abs (y - y1) == 2
  else if y - y1 == 0 then abs (x - x1) == 2
  else False

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
