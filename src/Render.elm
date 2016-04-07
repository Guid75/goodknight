module Render (render, renderMapToText, pokeLeftCell, pokeRightCell) where

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

        Cards.CardBack ->
            '?'

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


pokeLeftCell : ( Int, Int ) -> Cards.LandscapeCard -> RenderMap -> RenderMap
pokeLeftCell ( row, col ) { corners, edges, center } renderMap =
    let
        ( corner0, corner1, corner2 ) = corners

        ( edge0, edge1, edge2 ) = edges
    in
        renderPoke ( row, col + 4 ) (cornerToChar corner0) renderMap
            |> renderPoke ( row + 4, col + 8 ) (cornerToChar corner1)
            |> renderPoke ( row + 4, col ) (cornerToChar corner2)
            |> renderPoke ( row + 1, col + 5 ) '\\'
            |> renderPoke ( row + 2, col + 6 ) (edgeToChar False edge0)
            |> renderPoke ( row + 3, col + 7 ) '\\'
            |> renderPoke ( row + 1, col + 3 ) '/'
            |> renderPoke ( row + 2, col + 2 ) (edgeToChar True edge2)
            |> renderPoke ( row + 3, col + 1 ) '/'
            |> renderPoke ( row + 4, col + 1 ) '-'
            |> renderPoke ( row + 4, col + 2 ) '-'
            |> renderPoke ( row + 4, col + 3 ) '-'
            |> renderPoke ( row + 4, col + 4 ) (flatEdgeToChar edge1)
            |> renderPoke ( row + 4, col + 5 ) '-'
            |> renderPoke ( row + 4, col + 6 ) '-'
            |> renderPoke ( row + 4, col + 7 ) '-'
            |> renderPoke ( row + 2, col + 4 ) (landscapeToChar center)
            |> renderPoke ( row + 1, col + 4 ) ' '
            |> renderPoke ( row + 2, col + 3 ) ' '
            |> renderPoke ( row + 2, col + 5 ) ' '
            |> renderPoke ( row + 3, col + 2 ) ' '
            |> renderPoke ( row + 3, col + 3 ) ' '
            |> renderPoke ( row + 3, col + 4 ) ' '
            |> renderPoke ( row + 3, col + 5 ) ' '
            |> renderPoke ( row + 3, col + 6 ) ' '


renderLeftCell : ( Int, Int ) -> Maybe Cards.LandscapeCard -> RenderMap -> RenderMap
renderLeftCell ( x, y ) maybeCard renderMap =
    case maybeCard of
        Nothing ->
            renderMap

        Just card ->
            let
                row = y * 4

                col = x * 8 - (y * 4)
            in
                pokeLeftCell ( row, col ) card renderMap


pokeRightCell : ( Int, Int ) -> Cards.LandscapeCard -> RenderMap -> RenderMap
pokeRightCell ( row, col ) { corners, edges, center } renderMap =
    let
        ( corner0, corner1, corner2 ) = corners

        ( edge0, edge1, edge2 ) = edges
    in
        renderPoke ( row, 8 ) (cornerToChar corner0) renderMap
            |> renderPoke ( row + 4, col + 4 ) (cornerToChar corner1)
            |> renderPoke ( row, col ) (cornerToChar corner2)
            |> renderPoke ( row + 1, col + 7 ) '/'
            |> renderPoke ( row + 2, col + 6 ) (edgeToChar True edge0)
            |> renderPoke ( row + 3, col + 5 ) '/'
            |> renderPoke ( row + 3, col + 3 ) '\\'
            |> renderPoke ( row + 2, col + 2 ) (edgeToChar False edge1)
            |> renderPoke ( row + 1, col + 1 ) '\\'
            |> renderPoke ( row, col + 1 ) '-'
            |> renderPoke ( row, col + 2 ) '-'
            |> renderPoke ( row, col + 3 ) '-'
            |> renderPoke ( row, col + 4 ) (flatEdgeToChar edge2)
            |> renderPoke ( row, col + 5 ) '-'
            |> renderPoke ( row, col + 6 ) '-'
            |> renderPoke ( row, col + 7 ) '-'
            |> renderPoke ( row + 2, col + 4 ) (landscapeToChar center)
            |> renderPoke ( row + 1, col + 2 ) ' '
            |> renderPoke ( row + 1, col + 3 ) ' '
            |> renderPoke ( row + 1, col + 4 ) ' '
            |> renderPoke ( row + 1, col + 5 ) ' '
            |> renderPoke ( row + 1, col + 6 ) ' '
            |> renderPoke ( row + 2, col + 3 ) ' '
            |> renderPoke ( row + 2, col + 5 ) ' '
            |> renderPoke ( row + 3, col + 4 ) ' '


renderRightCell : ( Int, Int ) -> Maybe Cards.LandscapeCard -> RenderMap -> RenderMap
renderRightCell ( x, y ) maybeCard renderMap =
    case maybeCard of
        Nothing ->
            renderMap

        Just card ->
            let
                row = y * 4

                col = x * 8 - (y * 4) + 4
            in
                pokeRightCell ( row, col ) card renderMap


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


renderMapToText : ( Int, Int ) -> RenderMap -> String
renderMapToText ( x, y ) renderMap =
    let
        str = ""

        ( oldRowIndex, str' ) =
            Dict.filter (\rowIndex _ -> rowIndex >= y) renderMap
                |> Dict.foldl (renderRowsSince x) ( y - 1, str )
    in
        str'
