module Tests where

import ElmTest exposing (..)

import String
import Dict

import Board

boardSuite : Test
boardSuite =
  let
    yDict =
      Dict.singleton 0 ({ left = Just { landscapeIndex = 0, rotation = 0 }, right = Nothing })
    board =
      Dict.singleton 0 yDict
  in
    suite "Testing Board module"
            [ test "isEmptyCell should return false for (0, 0, CellLeft)"
                     (assert (not (Board.isEmptyCell (0, 0, Board.CellLeft) board)))
            , test "isEmptyCell should return true for (0, 0, CellRight)"
                     (assert (Board.isEmptyCell (0, 0, Board.CellRight) board))
            , test "isEmptyCell should return true for (0, 1, CellLeft)"
                     (assert (Board.isEmptyCell (0, 1, Board.CellLeft) board))
            ]

all : Test
all =
  suite "A Test Suite"
          [ boardSuite
          ]
