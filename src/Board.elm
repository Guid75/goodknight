module Board
    exposing
        ( CellPosition(..)
        , CellCoordinates
        , CellBinome
        , isEmptyCell
        , Board
        , Column
        , init
        , getLandscape
        , setLandscape
        , setCell
        )

import Array
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
    { landscapes : Grid LandscapeCard
    }


init : CellPosition -> Board
init pos =
    let
        binome =
            case pos of
                CellLeft ->
                    { left = Array.get 0 initialLandscapeDeck
                    , right = Nothing
                    }

                CellRight ->
                    { left = Nothing
                    , right = Array.get 0 initialLandscapeDeck
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


getLandscape : CellCoordinates -> Board -> Maybe LandscapeCard
getLandscape coord board =
    getCell coord board.landscapes


setLandscape : CellCoordinates -> LandscapeCard -> Board -> Board
setLandscape coord card board =
    { board | landscapes = (setCell coord card board.landscapes) }



-- initCellWithLandscapeCard : Board -> CellCoordinates -> Int -> Cell
-- initCellWithLandscapeCard board ( x, y, pos ) cardIndex =
--     let
--         maybeCard = Array.get cardIndex initialLandscapeDeck
--     in
--         case maybeCard of
--             Nothing ->
--                 {}
-- computeCellCard : Maybe Cell -> Maybe LandscapeCard
-- computeCellCard maybeCell =
--     case maybeCell of
--         Nothing ->
--             Nothing
--         Just cell ->
--             Array.get cell.landscapeIndex initialLandscapeDeck
--                 |> Maybe.map (rotateLandscapeCard cell.rotation)
