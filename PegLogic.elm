module PegLogic exposing (Board, move, randomBoard)

import Array exposing (..)

type Board = Array (Array Bool)

type Dir = L | R | U | D

move : (Int, Int) -> Dir -> Board -> Board
move (x, y) dir b = Debug.crash "hello"

solved : Board -> Board -> Bool
-- check if the given board equals the target board
solved = always True

randomBoard = identity
