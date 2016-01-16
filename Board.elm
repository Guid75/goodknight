module Board where

import Dict

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

getCellBinomeInBoard : (Int, Int) -> Board -> CellBinome
getCellBinomeInBoard (x, y) board =
  let
    yRaw = Dict.get x board
  in
    case yRaw of
      Just yDict ->
        case Dict.get y yDict of
          Just c -> c
          Nothing -> { left = Nothing, right = Nothing }
      Nothing -> { left = Nothing, right = Nothing }

getCellInBoard : CellCoordinates -> Board -> Maybe Cell
getCellInBoard (x, y, pos) board =
  let
    cellBinome = getCellBinomeInBoard (x, y) board
  in
    case pos of
      CellLeft -> cellBinome.left
      CellRight -> cellBinome.right

isEmptyCell : CellCoordinates -> Board -> Bool
isEmptyCell coord board =
  let
    cell = getCellInBoard coord board
  in
    case cell of
      Just cell -> False
      Nothing -> True

setCellInBoard : CellCoordinates -> Cell -> Board -> Board
setCellInBoard (x, y, pos) cell board =
  let
    yRaw = Dict.get x board
    oldCellBinome = getCellBinomeInBoard (x, y) board
    cellBinome = case pos of
             CellLeft -> { left = Just cell, right = oldCellBinome.right }
             CellRight -> { left = oldCellBinome.left, right = Just cell }
  in
    case yRaw of
      Just yDict -> Dict.insert x (Dict.insert y cellBinome yDict) board
      Nothing -> Dict.insert x (Dict.singleton y cellBinome) board
