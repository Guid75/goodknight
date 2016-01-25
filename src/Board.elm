module Board where

import Dict
import Maybe exposing (andThen)

import Cards exposing (..)

type CellPosition = CellLeft
                  | CellRight

type alias CellCoordinates = (Int, Int, CellPosition)

type alias Cell =
  { landscapeIndex : Int
  , rotation : Int
  }

type alias CellBinome =
  { left: Maybe Cell
  , right: Maybe Cell
  }

type alias Board =
  Dict.Dict Int (Dict.Dict Int CellBinome)

init : Board
init =
  Dict.singleton 0
        (Dict.singleton 0
               { left = Just { landscapeIndex = 0, rotation = 0 }
               , right = Nothing }
        )

getCellBinome : (Int, Int) -> Board -> Maybe CellBinome
getCellBinome (x, y) board =
  Dict.get x board `andThen` Dict.get y

getCell : CellCoordinates -> Board -> Maybe Cell
getCell (x, y, pos) board =
  let
    maybeBinome = getCellBinome (x, y) board
  in
    case pos of
      CellLeft -> maybeBinome `andThen` .left
      CellRight -> maybeBinome `andThen` .right

isEmptyCell : CellCoordinates -> Board -> Bool
isEmptyCell coord board =
  case getCell coord board of
    Just cell -> False
    Nothing -> True

setCell : CellCoordinates -> Cell -> Board -> Board
setCell (x, y, pos) cell board =
  let
    maybeColumn = Dict.get x board
    binome = getCellBinome (x, y) board
    newBinome =
      case pos of
        CellLeft -> { left = Just cell, right = binome `andThen` .right }
        CellRight -> { left = binome `andThen` .left, right = Just cell }
  in
    case maybeColumn of
      Just yDict -> Dict.insert x (Dict.insert y newBinome yDict) board
      Nothing -> Dict.insert x (Dict.singleton y newBinome) board
