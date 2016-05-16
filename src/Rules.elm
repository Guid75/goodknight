module Rules exposing (..)

import Debug
import Maybe
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
        , getMaybeSide ( x, y - 1, CellRight ) 2
        , getMaybeSide ( x - 1, y, CellRight ) 0
        )

      CellRight ->
        ( getMaybeSide ( x + 1, y, CellLeft ) 2
        , getMaybeSide ( x, y, CellLeft ) 0
        , getMaybeSide ( x, y + 1, CellLeft ) 1
        )


isPossibleMove : CellCoordinates -> LandscapeCard -> Int -> Board -> Bool
isPossibleMove ( x, y, pos ) card rot board =
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
      getSurroundingSides board ( x, y, pos )
  in
    (isEmptyCell ( x, y, pos ) board.landscapes)
      && (not (List.all ((==) Nothing) [ s0, s1, s2 ]))
      && (compatibleSides side0 s0)
      && (compatibleSides side1 s1)
      && (compatibleSides side2 s2)
