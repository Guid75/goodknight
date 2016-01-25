module BoardTestSuite (general) where

import ElmTest exposing (..)

import Dict

import Board

general : Test
general =
  let
    column =
      Dict.singleton 0 ({ left = Just { landscapeIndex = 0, rotation = 0 }, right = Nothing })
    board =
      Dict.singleton 0 column
  in
    suite "Testing Board module"
            [ test "isEmptyCell should return false for (0, 0, CellLeft)"
                     (assert (not (Board.isEmptyCell (0, 0, Board.CellLeft) board)))
            , test "isEmptyCell should return true for (0, 0, CellRight)"
                     (assert (Board.isEmptyCell (0, 0, Board.CellRight) board))
            , test "isEmptyCell should return true for (0, 1, CellLeft)"
                     (assert (Board.isEmptyCell (0, 1, Board.CellLeft) board))
            , test "getCellBinome should return Just a binome for (0, 0)"
                     (assertNotEqual Nothing (Board.getCellBinome (0, 0) board))
            , test "getCellBinome should return Nothing for (0, 1)"
                     (assertEqual Nothing (Board.getCellBinome (0, 1) board))
            ]
