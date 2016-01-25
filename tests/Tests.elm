module Tests where

import ElmTest exposing (..)

import String
import Dict

import Cards
import Board
import Rules

cardsSuite : Test
cardsSuite =
    suite "Testing Cards module"
            [ test "shift3 should return (1, 2, 3) for (1, 2, 3) with a tick value of 0"
                     (assertEqual (Cards.shift3 0 (1, 2, 3)) (1, 2, 3))
            , test "shift3 should return (3, 1, 2) for (1, 2, 3) with a tick value of 1"
                     (assertEqual (Cards.shift3 1 (1, 2, 3)) (3, 1, 2))
            , test "shift3 should return (2, 3, 1) for (1, 2, 3) with a tick value of 2"
                     (assertEqual (Cards.shift3 2 (1, 2, 3)) (2, 3, 1))
            , test "shift3 should return (1, 2, 3) for (1, 2, 3) with a tick value of 3"
                     (assertEqual (Cards.shift3 3 (1, 2, 3)) (1, 2, 3))
            , test "shift3 should return (2, 3, 1) for (1, 2, 3) with a tick value of -1"
                     (assertEqual (Cards.shift3 -1 (1, 2, 3)) (2, 3, 1))
            , test "shift3 should return (3, 1, 2) for (1, 2, 3) with a tick value of -2"
                     (assertEqual (Cards.shift3 -2 (1, 2, 3)) (3, 1, 2))
            ]

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
          [ cardsSuite
          , boardSuite
          , rulesSuite
          ]
