module RulesTestSuite (general) where

import ElmTest exposing (..)

import Dict

import Board
import Rules

general : Test
general =
  let
    column =
      Dict.singleton 0 ({ left = Just { landscapeIndex = 0, rotation = 0 }, right = Nothing })
    board =
      Dict.singleton 0 column
  in
    suite "Testing Rules module"
            [ test "isPossibleMove should return true for this card (2) and this rotation (3)"
                     (assert (Rules.isPossibleMove
                                    (0, 1, Board.CellRight)
                                    2  3  board))
            , test "isPossibleMove should return false for this card (2) and this rotation (0)"
                     (assert (not (Rules.isPossibleMove
                                    (0, 0, Board.CellRight)
                                    2  0  board)))
            ]
