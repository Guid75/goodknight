module RulesTestSuite (general) where

import ElmTest exposing (..)
import Dict
import Cards exposing (LandscapeItem(..), allNeutral)
import Board
import Rules


isPossibleMoveOnRight : Test
isPossibleMoveOnRight =
    let
        board = Board.init Board.CellLeft
    in
        suite
            "Testing isPossibleMove on a right cell"
            [ test
                "isPossibleMove should return true for this card (2) and this rotation (0)"
                (assert
                    (Rules.isPossibleMove
                        ( 0, -1, Board.CellRight )
                        { corners = ( Tournament, Neutral, Neutral )
                        , edges = allNeutral
                        , center = Neutral
                        }
                        0
                        board
                    )
                )
            , test
                "isPossibleMove should return true for this card (2) and this rotation (1)"
                (assert
                    (Rules.isPossibleMove
                        ( 0, 0, Board.CellRight )
                        { corners = ( Tournament, Neutral, Neutral )
                        , edges = allNeutral
                        , center = Neutral
                        }
                        1
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


isPossibleMoveOnLeft : Test
isPossibleMoveOnLeft =
    let
        board = Board.init Board.CellRight
    in
        suite
            "Testing isPossibleMove on a left cell"
            [ test
                "isPossibleMove should return true for this card (2) and this rotation (0)"
                (assert
                    (not
                        (Rules.isPossibleMove
                            ( 0, 0, Board.CellLeft )
                            { corners = ( Tournament, Neutral, Neutral )
                            , edges = allNeutral
                            , center = Neutral
                            }
                            0
                            board
                        )
                    )
                )
            , test
                "isPossibleMove should return true for this card (2) and this rotation (1)"
                (assert
                    (Rules.isPossibleMove
                        ( 0, 1, Board.CellLeft )
                        { corners = ( Tournament, Neutral, Neutral )
                        , edges = allNeutral
                        , center = Neutral
                        }
                        0
                        board
                    )
                )
            , test
                "isPossibleMove should return false for this card (2) and this rotation (0)"
                (assert
                    (Rules.isPossibleMove
                        ( 1, 0, Board.CellLeft )
                        { corners = ( Tournament, Neutral, Neutral )
                        , edges = allNeutral
                        , center = Neutral
                        }
                        2
                        board
                    )
                )
            ]


general : Test
general =
    suite
        "Testing Rules module"
        [ isPossibleMoveOnLeft
        , isPossibleMoveOnRight
        ]
