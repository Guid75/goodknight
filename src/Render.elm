module Render (render, renderMapToHtml, pokeLeftCell, pokeRightCell) where

import Cards
import Board
import List
import String
import Dict exposing (Dict)
import Color exposing (Color)
import Html exposing (Html, text, span, pre)
import Html.Attributes exposing (..)


-- indexed by rows, then by columns


type alias RenderPixel =
    { char : Char
    , color : Color
    }


type alias RenderRow =
    Dict Int RenderPixel


type alias RenderMap =
    Dict Int RenderRow


pokePixel : ( Int, Int ) -> RenderPixel -> RenderMap -> RenderMap
pokePixel ( row, col ) pixel renderMap =
    let
        maybeRow = Dict.get row renderMap
    in
        case maybeRow of
            Just rowMap ->
                Dict.insert row (Dict.insert col pixel rowMap) renderMap

            Nothing ->
                Dict.insert row (Dict.singleton col pixel) renderMap


pokeMonoPixel : ( Int, Int ) -> Char -> RenderMap -> RenderMap
pokeMonoPixel pos c renderMap =
    pokePixel pos { char = c, color = Color.black } renderMap


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
pokeLeftCell ( row, col ) card renderMap =
    let
        ( corner0, corner1, corner2 ) = card.corners

        ( edge0, edge1, edge2 ) = card.edges
    in
        renderMap
            |> pokeMonoPixel ( row, col + 4 ) (cornerToChar corner0)
            |> pokeMonoPixel ( row + 4, col + 8 ) (cornerToChar corner1)
            |> pokeMonoPixel ( row + 4, col ) (cornerToChar corner2)
            |> pokeMonoPixel ( row + 1, col + 5 ) '\\'
            |> pokePixel ( row + 2, col + 6 ) { char = (edgeToChar False edge0), color = Color.green }
            |> pokeMonoPixel ( row + 3, col + 7 ) '\\'
            |> pokeMonoPixel ( row + 1, col + 3 ) '/'
            |> pokePixel ( row + 2, col + 2 ) { char = (edgeToChar True edge2), color = Color.blue }
            |> pokeMonoPixel ( row + 3, col + 1 ) '/'
            |> pokeMonoPixel ( row + 4, col + 1 ) '-'
            |> pokeMonoPixel ( row + 4, col + 2 ) '-'
            |> pokeMonoPixel ( row + 4, col + 3 ) '-'
            |> pokePixel ( row + 4, col + 4 ) { char = (flatEdgeToChar edge1), color = Color.red }
            |> pokeMonoPixel ( row + 4, col + 5 ) '-'
            |> pokeMonoPixel ( row + 4, col + 6 ) '-'
            |> pokeMonoPixel ( row + 4, col + 7 ) '-'
            |> pokePixel ( row + 2, col + 4 ) { char = (landscapeToChar card.center), color = Color.red }
            |> pokeMonoPixel ( row + 1, col + 4 ) ' '
            |> pokeMonoPixel ( row + 2, col + 3 ) ' '
            |> pokeMonoPixel ( row + 2, col + 5 ) ' '
            |> pokeMonoPixel ( row + 3, col + 2 ) ' '
            |> pokeMonoPixel ( row + 3, col + 3 ) ' '
            |> pokeMonoPixel ( row + 3, col + 4 ) ' '
            |> pokeMonoPixel ( row + 3, col + 5 ) ' '
            |> pokeMonoPixel ( row + 3, col + 6 ) ' '


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
pokeRightCell ( row, col ) card renderMap =
    let
        ( corner0, corner1, corner2 ) = card.corners

        ( edge0, edge1, edge2 ) = card.edges
    in
        renderMap
            |> pokeMonoPixel ( row, 8 ) (cornerToChar corner0)
            |> pokeMonoPixel ( row + 4, col + 4 ) (cornerToChar corner1)
            |> pokeMonoPixel ( row, col ) (cornerToChar corner2)
            |> pokeMonoPixel ( row + 1, col + 7 ) '/'
            |> pokeMonoPixel ( row + 2, col + 6 ) (edgeToChar True edge0)
            |> pokeMonoPixel ( row + 3, col + 5 ) '/'
            |> pokeMonoPixel ( row + 3, col + 3 ) '\\'
            |> pokeMonoPixel ( row + 2, col + 2 ) (edgeToChar False edge1)
            |> pokeMonoPixel ( row + 1, col + 1 ) '\\'
            |> pokeMonoPixel ( row, col + 1 ) '-'
            |> pokeMonoPixel ( row, col + 2 ) '-'
            |> pokeMonoPixel ( row, col + 3 ) '-'
            |> pokeMonoPixel ( row, col + 4 ) (flatEdgeToChar edge2)
            |> pokeMonoPixel ( row, col + 5 ) '-'
            |> pokeMonoPixel ( row, col + 6 ) '-'
            |> pokeMonoPixel ( row, col + 7 ) '-'
            |> pokeMonoPixel ( row + 2, col + 4 ) (landscapeToChar card.center)
            |> pokeMonoPixel ( row + 1, col + 2 ) ' '
            |> pokeMonoPixel ( row + 1, col + 3 ) ' '
            |> pokeMonoPixel ( row + 1, col + 4 ) ' '
            |> pokeMonoPixel ( row + 1, col + 5 ) ' '
            |> pokeMonoPixel ( row + 1, col + 6 ) ' '
            |> pokeMonoPixel ( row + 2, col + 3 ) ' '
            |> pokeMonoPixel ( row + 2, col + 5 ) ' '
            |> pokeMonoPixel ( row + 3, col + 4 ) ' '


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


crHtml : Html
crHtml =
    text "\n"


renderPixelsSince : Int -> RenderPixel -> ( Int, List Html ) -> ( Int, List Html )
renderPixelsSince colIndex pixel ( oldColIndex, renderItems ) =
    let
        emptyPixelsCount = colIndex - oldColIndex - 1

        emptyPixels = List.repeat emptyPixelsCount (text " ")

        colors = Color.toRgb pixel.color
    in
        ( colIndex
        , List.concat
            [ renderItems
            , emptyPixels
            , [ span
                    [ style
                        [ ( "color"
                          , "rgb("
                                ++ (toString colors.red)
                                ++ ","
                                ++ (toString colors.green)
                                ++ ","
                                ++ (toString colors.blue)
                                ++ ")"
                          )
                        ]
                    ]
                    [ text (String.fromChar pixel.char) ]
              ]
            ]
        )


renderRow : Int -> RenderRow -> List Html
renderRow x row =
    let
        ( _, renderItems ) =
            Dict.filter (\colIndex _ -> colIndex >= x) row
                |> Dict.foldl renderPixelsSince ( x - 1, [] )
    in
        List.concat [ renderItems, [ crHtml ] ]


renderRowsSince : Int -> Int -> RenderRow -> ( Int, List Html ) -> ( Int, List Html )
renderRowsSince x rowIndex row ( oldRowIndex, renderItems ) =
    let
        emptyLinesCount = rowIndex - oldRowIndex - 1

        emptyLines = List.repeat emptyLinesCount crHtml
    in
        ( rowIndex, List.concat [ renderItems, emptyLines, (renderRow x row) ] )


renderMapToHtml : ( Int, Int ) -> RenderMap -> List Html
renderMapToHtml ( x, y ) renderMap =
    let
        ( oldRowIndex, renderItems ) =
            Dict.filter (\rowIndex _ -> rowIndex >= y) renderMap
                |> Dict.foldl (renderRowsSince x) ( y - 1, [] )
    in
        renderItems



----------------------------------------------------
-- renderRowsSince2 : Int -> (Int, Int) -> RenderMap -> String
-- renderRowsSince2 left
-- renderMapToText2 : ( Int, Int ) -> ( Int, Int ) -> RenderMap -> String
-- renderMapToText2 ( left, top ) ( width, height ) renderMap =
--     let
--         str = ""
--         ( oldRowIndex, str' ) =
--             Dict.filter (\rowIndex _ -> rowIndex >= top && rowIndex <= top + height) renderMap
--                 |> Dict.foldl (renderRowsSince2 left) ( top - 1, str )
--     in
--         str'
