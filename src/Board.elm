module Board
    exposing
        ( CellPosition(..)
        , CellCoordinates
        , CellBinome
        , isEmptyCell
        , Board
        , Column
        , init
        , getLandscapeCard
        , setLandscapeCard
        )

import Dict exposing (Dict)
import Maybe
import Cards exposing (..)


type CellPosition
    = CellLeft
    | CellRight


type alias CellCoordinates =
    ( Int, Int, CellPosition )


type alias CellBinome a =
    { left : Maybe a
    , right : Maybe a
    }


type alias Column a =
    Dict Int (CellBinome a)


type alias Grid a =
    Dict Int (Column a)


type alias Board =
    { landscapes : Grid LandscapeCard }


init : CellPosition -> Int -> Board
init pos rot =
    let
        binome =
            case pos of
                CellLeft ->
                    { left = Just <| rotateLandscapeCard rot castleCard
                    , right = Nothing
                    }

                CellRight ->
                    { left = Nothing
                    , right = Just <| rotateLandscapeCard rot castleCard
                    }
    in
        { landscapes =
            Dict.singleton 0
                (Dict.singleton 0
                    binome
                )
        }


getCellBinome : ( Int, Int ) -> Grid a -> Maybe (CellBinome a)
getCellBinome ( x, y ) grid =
    Dict.get x grid `Maybe.andThen` Dict.get y


getCell : CellCoordinates -> Grid a -> Maybe a
getCell ( x, y, pos ) grid =
    let
        maybeBinome =
            getCellBinome ( x, y ) grid
    in
        case pos of
            CellLeft ->
                maybeBinome `Maybe.andThen` .left

            CellRight ->
                maybeBinome `Maybe.andThen` .right


isEmptyCell : CellCoordinates -> Grid a -> Bool
isEmptyCell coord grid =
    (getCell coord grid) == Nothing


setCell : CellCoordinates -> a -> Grid a -> Grid a
setCell ( x, y, pos ) item grid =
    let
        maybeColumn =
            Dict.get x grid

        binome =
            getCellBinome ( x, y ) grid

        newBinome =
            case pos of
                CellLeft ->
                    { left = Just item, right = binome `Maybe.andThen` .right }

                CellRight ->
                    { left = binome `Maybe.andThen` .left, right = Just item }
    in
        case maybeColumn of
            Just yDict ->
                Dict.insert x (Dict.insert y newBinome yDict) grid

            Nothing ->
                Dict.insert x (Dict.singleton y newBinome) grid


getLandscapeCard : CellCoordinates -> Board -> Maybe LandscapeCard
getLandscapeCard coord board =
    getCell coord board.landscapes


setLandscapeCard : CellCoordinates -> LandscapeCard -> Board -> Board
setLandscapeCard coord card board =
    { board | landscapes = (setCell coord card board.landscapes) }
