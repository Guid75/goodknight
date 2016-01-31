module Rules (..) where

import Debug
import Maybe
import Cards exposing (..)
import Board exposing (..)


isPossibleMove : CellCoordinates -> LandscapeCard -> Int -> Board -> Bool
isPossibleMove ( x, y, pos ) card rot board =
    let
        rotatedCard = rotateLandscapeCard rot card

        side0 = getLandscapeSide 0 rotatedCard

        side1 = getLandscapeSide 1 rotatedCard

        side2 = getLandscapeSide 2 rotatedCard
    in
        case pos of
            CellLeft ->
                let
                    westSide =
                        getLandscape ( x - 1, y, CellRight ) board
                            |> Maybe.map (getLandscapeSide 0)

                    eastSide =
                        getLandscape ( x, y, CellRight ) board
                            |> Maybe.map (getLandscapeSide 1)

                    southSide =
                        getLandscape ( x, y - 1, CellRight ) board
                            |> Maybe.map (getLandscapeSide 2)
                in
                    (not (List.all ((==) Nothing) [ eastSide, westSide, southSide ]))
                        && (compatibleSides side2 westSide)
                        && (compatibleSides side0 eastSide)
                        && (compatibleSides side1 southSide)

            CellRight ->
                let
                    westSide =
                        getLandscape ( x, y, CellLeft ) board
                            |> Maybe.map (getLandscapeSide 0)

                    eastSide =
                        getLandscape ( x + 1, y, CellLeft ) board
                            |> Maybe.map (getLandscapeSide 2)

                    northSide =
                        getLandscape ( x, y + 1, CellLeft ) board
                            |> Maybe.map (getLandscapeSide 1)
                in
                    (not (List.all ((==) Nothing) [ eastSide, westSide, northSide ]))
                        && (compatibleSides side2 westSide)
                        && (compatibleSides side1 eastSide)
                        && (compatibleSides side0 northSide)
