module BoardTestSuite (general) where

import ElmTest exposing (..)
import Dict
import Board


general : Test
general =
    let
        board = Board.init Board.CellLeft
    in
        suite
            "Testing Board module"
            [ test
                "isEmptyCell should return false for (0, 0, CellLeft)"
                (assert (not (Board.isEmptyCell ( 0, 0, Board.CellLeft ) board.landscapes)))
            , test
                "isEmptyCell should return true for (0, 0, CellRight)"
                (assert (Board.isEmptyCell ( 0, 0, Board.CellRight ) board.landscapes))
            , test
                "isEmptyCell should return true for (0, 1, CellLeft)"
                (assert (Board.isEmptyCell ( 0, 1, Board.CellLeft ) board.landscapes))
            , test
                "getCellBinome should return Just a binome for (0, 0)"
                (assertNotEqual Nothing (Board.getCellBinome ( 0, 0 ) board.landscapes))
            , test
                "getCellBinome should return Nothing for (0, 1)"
                (assertEqual Nothing (Board.getCellBinome ( 0, 1 ) board.landscapes))
            ]
