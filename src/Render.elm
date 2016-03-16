module Render (render, renderMapToText) where

import Cards
import Board
import List
import String
import Dict exposing (Dict)


-- indexed by rows, then by columns


type alias RenderRow =
    Dict Int Char


type alias RenderMap =
    Dict Int RenderRow


renderPoke : ( Int, Int ) -> Char -> RenderMap -> RenderMap
renderPoke ( row, col ) c renderMap =
    let
        maybeRow = Dict.get row renderMap
    in
        case maybeRow of
            Just rowMap ->
                Dict.insert row (Dict.insert col c rowMap) renderMap

            Nothing ->
                Dict.insert row (Dict.singleton col c) renderMap


landscapeToChar : Cards.LandscapeItem -> Char
landscapeToChar item =
    case item of
        Cards.Castle ->
            'C'

        Cards.Horse ->
            'H'

        _ ->
            ' '


cornerToChar : Cards.LandscapeItem -> Char
cornerToChar item =
    case item of
        Cards.Neutral ->
            '.'

        Cards.Tournament ->
            'T'

        _ ->
            '#'


colorToChar : Cards.Color -> Char
colorToChar color =
    case color of
        Cards.Red ->
            'R'

        Cards.Yellow ->
            'Y'

        Cards.Blue ->
            'B'

        Cards.Green ->
            'G'


edgeToChar : Bool -> Cards.LandscapeItem -> Char
edgeToChar mounting item =
    case item of
        Cards.Neutral ->
            case mounting of
                True ->
                    '/'

                False ->
                    '\\'

        Cards.Cross color ->
            colorToChar color

        _ ->
            '#'


flatEdgeToChar : Cards.LandscapeItem -> Char
flatEdgeToChar item =
    case item of
        Cards.Neutral ->
            '-'

        Cards.Cross color ->
            colorToChar color

        _ ->
            '#'


renderLeftCell : ( Int, Int ) -> Maybe Cards.LandscapeCard -> RenderMap -> RenderMap
renderLeftCell ( x, y ) maybeCard renderMap =
    let
        toAbsoluteCoord row col =
            ( row + y * 4, col + x * 8 - (y * 4) )
    in
        case maybeCard of
            Nothing ->
                renderMap

            Just { corners, edges, center } ->
                let
                    ( corner0, corner1, corner2 ) = corners

                    ( edge0, edge1, edge2 ) = edges
                in
                    renderPoke (toAbsoluteCoord 0 4) (cornerToChar corner0) renderMap
                        |> renderPoke (toAbsoluteCoord 4 8) (cornerToChar corner1)
                        |> renderPoke (toAbsoluteCoord 4 0) (cornerToChar corner2)
                        |> renderPoke (toAbsoluteCoord 1 5) '\\'
                        |> renderPoke (toAbsoluteCoord 2 6) (edgeToChar False edge0)
                        |> renderPoke (toAbsoluteCoord 3 7) '\\'
                        |> renderPoke (toAbsoluteCoord 1 3) '/'
                        |> renderPoke (toAbsoluteCoord 2 2) (edgeToChar True edge2)
                        |> renderPoke (toAbsoluteCoord 3 1) '/'
                        |> renderPoke (toAbsoluteCoord 4 1) '-'
                        |> renderPoke (toAbsoluteCoord 4 2) '-'
                        |> renderPoke (toAbsoluteCoord 4 3) '-'
                        |> renderPoke (toAbsoluteCoord 4 4) (flatEdgeToChar edge1)
                        |> renderPoke (toAbsoluteCoord 4 5) '-'
                        |> renderPoke (toAbsoluteCoord 4 6) '-'
                        |> renderPoke (toAbsoluteCoord 4 7) '-'
                        |> renderPoke (toAbsoluteCoord 2 4) (landscapeToChar center)


renderRightCell : ( Int, Int ) -> Maybe Cards.LandscapeCard -> RenderMap -> RenderMap
renderRightCell ( x, y ) maybeCard renderMap =
    let
        toAbsoluteCoord row col =
            ( row + y * 4, col + x * 8 - (y * 4) + 4 )
    in
        case maybeCard of
            Nothing ->
                renderMap

            Just { corners, edges, center } ->
                let
                    ( corner0, corner1, corner2 ) = corners

                    ( edge0, edge1, edge2 ) = edges
                in
                    renderPoke (toAbsoluteCoord 0 8) (cornerToChar corner0) renderMap
                        |> renderPoke (toAbsoluteCoord 4 4) (cornerToChar corner1)
                        |> renderPoke (toAbsoluteCoord 0 0) (cornerToChar corner2)
                        |> renderPoke (toAbsoluteCoord 1 7) '/'
                        |> renderPoke (toAbsoluteCoord 2 6) (edgeToChar True edge0)
                        |> renderPoke (toAbsoluteCoord 3 5) '/'
                        |> renderPoke (toAbsoluteCoord 3 3) '\\'
                        |> renderPoke (toAbsoluteCoord 2 2) (edgeToChar False edge1)
                        |> renderPoke (toAbsoluteCoord 1 1) '\\'
                        |> renderPoke (toAbsoluteCoord 0 1) '-'
                        |> renderPoke (toAbsoluteCoord 0 2) '-'
                        |> renderPoke (toAbsoluteCoord 0 3) '-'
                        |> renderPoke (toAbsoluteCoord 0 4) (flatEdgeToChar edge2)
                        |> renderPoke (toAbsoluteCoord 0 5) '-'
                        |> renderPoke (toAbsoluteCoord 0 6) '-'
                        |> renderPoke (toAbsoluteCoord 0 7) '-'
                        |> renderPoke (toAbsoluteCoord 2 4) (landscapeToChar center)


renderCell : Board.CellCoordinates -> Board.CellBinome Cards.LandscapeCard -> RenderMap -> RenderMap
renderCell ( x, y, position ) cellBinome renderMap =
    case position of
        Board.CellLeft ->
            renderLeftCell ( x, y ) cellBinome.left renderMap

        Board.CellRight ->
            renderRightCell ( x, y ) cellBinome.right renderMap


render : Board.Board -> RenderMap
render board =
    let
        renderBinome x y cellBinome renderMap =
            renderCell ( x, y, Board.CellLeft ) cellBinome renderMap
                |> renderCell ( x, y, Board.CellRight ) cellBinome

        renderColumn : Int -> Board.Column Cards.LandscapeCard -> RenderMap -> RenderMap
        renderColumn x column renderMap =
            Dict.foldl (renderBinome x) renderMap column
    in
        Dict.foldl renderColumn Dict.empty board.landscapes


renderCharsSince : Int -> Char -> ( Int, String ) -> ( Int, String )
renderCharsSince colIndex char ( oldColIndex, str ) =
    let
        withEmptyChars = str ++ String.repeat (colIndex - oldColIndex - 1) " "
    in
        ( colIndex, withEmptyChars ++ (String.fromChar char) )


renderRow : Int -> RenderRow -> String
renderRow x row =
    let
        ( oldColIndex, str' ) =
            Dict.filter (\colIndex _ -> colIndex >= x) row
                |> Dict.foldl renderCharsSince ( x - 1, "" )
    in
        str'


renderRowsSince : Int -> Int -> RenderRow -> ( Int, String ) -> ( Int, String )
renderRowsSince x rowIndex row ( oldRowIndex, str ) =
    let
        emptyLines = [oldRowIndex + 1..rowIndex - 1]

        withEmptyLines = List.foldl (\rowIndex str -> str ++ "\n") str emptyLines
    in
        ( rowIndex, withEmptyLines ++ (renderRow x row) ++ "\n" )


renderMapToText : RenderMap -> ( Int, Int ) -> String
renderMapToText renderMap ( x, y ) =
    let
        str = ""

        ( oldRowIndex, str' ) =
            Dict.filter (\rowIndex _ -> rowIndex >= y) renderMap
                |> Dict.foldl (renderRowsSince x) ( y - 1, str )
    in
        str'
