module Tests where

import ElmTest exposing (..)

import String
import Dict

import Board
import Rules

boardSuite : Test
boardSuite =
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

rulesSuite : Test
rulesSuite =
  let
    column =
      Dict.singleton 0 ({ left = Just { landscapeIndex = 0, rotation = 0 }, right = Nothing })
    board =
      Dict.singleton 0 column
  in
    suite "Testing Board module"
            [ test "isPossibleMove should return true for this card (2) and this rotation (3)"
                     (assert (Rules.isPossibleMove
                                    (0, 1, Board.CellRight)
                                    2  3  board))
            , test "isPossibleMove should return false for this card (2) and this rotation (0)"
                     (assert (not (Rules.isPossibleMove
                                    (0, 0, Board.CellRight)
                                    2  0  board)))
            ]

all : Test
all =
  suite "A Test Suite"
          [ boardSuite
          , rulesSuite
          ]
