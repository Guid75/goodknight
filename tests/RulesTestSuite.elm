module RulesTestSuite (general) where

import ElmTest exposing (..)
import Dict
import Cards exposing (LandscapeItem(..), allNeutral)
import Board
import Rules


general : Test
general =
    let
        board = Board.init
    in
        suite
            "Testing Rules module"
            [ test
                "isPossibleMove should return true for this card (2) and this rotation (1)"
                (assert
                    (Rules.isPossibleMove
                        ( 0, -1, Board.CellRight )
                        { corners = ( Tournament, Neutral, Neutral )
                        , edges = allNeutral
                        , center = Neutral
                        }
                        1
                        board
                    )
                )
            , test
                "isPossibleMove should return true for this card (2) and this rotation (2)"
                (assert
                    (Rules.isPossibleMove
                        ( 0, 0, Board.CellRight )
                        { corners = ( Tournament, Neutral, Neutral )
                        , edges = allNeutral
                        , center = Neutral
                        }
                        2
                        board
                    )
                )
            , test
                "isPossibleMove should return false for this card (2) and this rotation (0)"
                (assert
                    (not
                        (Rules.isPossibleMove
                            ( 0, 0, Board.CellRight )
                            { corners = ( Tournament, Neutral, Neutral )
                            , edges = allNeutral
                            , center = Neutral
                            }
                            0
                            board
                        )
                    )
                )
            ]
