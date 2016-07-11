module Rules exposing (..)

import Debug
import Dict
import Maybe
import Array
import Cards exposing (..)
import Board exposing (..)


getSurroundingSides : Board -> CellCoordinates -> ( Maybe LandscapeSide, Maybe LandscapeSide, Maybe LandscapeSide )
getSurroundingSides board ( x, y, pos ) =
    let
        getMaybeSide ( x, y, pos ) sideIndex =
            getLandscape ( x, y, pos ) board |> Maybe.map (getLandscapeSide sideIndex)
    in
        case pos of
            CellLeft ->
                ( getMaybeSide ( x, y, CellRight ) 1
                , getMaybeSide ( x, y + 1, CellRight ) 2
                , getMaybeSide ( x - 1, y, CellRight ) 0
                )

            CellRight ->
                ( getMaybeSide ( x + 1, y, CellLeft ) 2
                , getMaybeSide ( x, y, CellLeft ) 0
                , getMaybeSide ( x, y - 1, CellLeft ) 1
                )


isPossibleMove : Board -> LandscapeCard -> CellCoordinates -> Int -> Bool
isPossibleMove board card ( col, row, pos ) rot =
    let
        rotatedCard =
            rotateLandscapeCard rot card

        side0 =
            getLandscapeSide 0 rotatedCard

        side1 =
            getLandscapeSide 1 rotatedCard

        side2 =
            getLandscapeSide 2 rotatedCard

        ( s0, s1, s2 ) =
            getSurroundingSides board ( col, row, pos )
    in
        (isEmptyCell ( col, row, pos ) board.landscapes)
            && (not (List.all ((==) Nothing) [ s0, s1, s2 ]))
            && (compatibleSides side0 s0)
            && (compatibleSides side1 s1)
            && (compatibleSides side2 s2)


type alias Move =
    { coord : CellCoordinates
    , rots : List Int
    , currentRot : Int
    }


getPossibleRotationsByCoordinates : Board -> LandscapeCard -> CellCoordinates -> List Int
getPossibleRotationsByCoordinates board card coord =
    let
        isPossibleMove' : Int -> Bool
        isPossibleMove' =
            isPossibleMove board card coord

        ( col, row, pos ) =
            coord
    in
        List.filter isPossibleMove' [0..2]


getPossibleMovesArroundCell : Board -> LandscapeCard -> CellCoordinates -> List Move
getPossibleMovesArroundCell board card ( col, row, pos ) =
    let
        coordsToCheck =
            (if pos == CellLeft then
                [ ( col - 1, row, CellRight )
                , ( col, row, CellRight )
                , ( col, row + 1, CellRight )
                ]
             else
                [ ( col, row, CellLeft )
                , ( col + 1, row, CellLeft )
                , ( col, row - 1, CellLeft )
                ]
            )

        coordToMoves : CellCoordinates -> List Move -> List Move
        coordToMoves coord moves =
            case getPossibleRotationsByCoordinates board card coord of
                [] ->
                    moves

                rots ->
                    { coord = coord, rots = rots, currentRot = 0 } :: moves
    in
        List.foldl coordToMoves [] coordsToCheck


getPossibleMovesArroundBinome : Board -> LandscapeCard -> ( Int, Int ) -> List Move
getPossibleMovesArroundBinome board card ( col, row ) =
    (getPossibleMovesArroundCell board card ( col, row, CellLeft ))
        ++ (getPossibleMovesArroundCell board card ( col, row, CellRight ))


getPossibleMoves : Board -> LandscapeCard -> List Move
getPossibleMoves board card =
    let
        getPossibleMovesForCol : Int -> Column LandscapeCard -> List Move -> List Move
        getPossibleMovesForCol col column moves =
            let
                getPossibleMovesForBinome col row _ moves =
                    moves ++ getPossibleMovesArroundBinome board card ( col, row )
            in
                Dict.foldl (getPossibleMovesForBinome col) moves column
    in
        Dict.foldl getPossibleMovesForCol [] board.landscapes


movesToBoard : List Move -> LandscapeCard -> Board
movesToBoard moves card =
    let
        pokeCard move board =
            let
                nthRot =
                    Array.fromList move.rots
                        |> Array.get move.currentRot
                        |> Maybe.withDefault 0
            in
                setLandscape move.coord (rotateLandscapeCard nthRot card) board
    in
        List.foldr pokeCard { landscapes = Dict.empty } moves
