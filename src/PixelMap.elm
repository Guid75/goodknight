module PixelMap
    exposing
        ( PixelMap
        , PixelRow
        , Pixel
        , pixelizeBoard
        , pixelizeCell
        , pokeLeftCell
        , pokePixel
        , pokeRightCell
        , grayIt
        )

import Cards exposing (LandscapeCard)
import Board
import Dict exposing (Dict)
import Color exposing (Color)


-- indexed by rows, then by columns


type alias Pixel =
    { char : Char
    , color : Color
    }


blackPixel : Char -> Pixel
blackPixel c =
    { char = c, color = Color.black }


type alias PixelRow =
    Dict Int Pixel


type alias PixelMap =
    Dict Int PixelRow


pokePixel : ( Int, Int ) -> Pixel -> PixelMap -> PixelMap
pokePixel ( row, col ) pixel pixelMap =
    let
        maybeRow =
            Dict.get row pixelMap
    in
        case maybeRow of
            Just pixelRow ->
                Dict.insert row (Dict.insert col pixel pixelRow) pixelMap

            Nothing ->
                Dict.insert row (Dict.singleton col pixel) pixelMap


pokeMonoPixel : ( Int, Int ) -> Char -> PixelMap -> PixelMap
pokeMonoPixel pos c pixelMap =
    pokePixel pos (blackPixel c) pixelMap


landscapeToPixel : Cards.LandscapeItem -> Pixel
landscapeToPixel item =
    case item of
        Cards.Castle ->
            blackPixel 'C'

        Cards.Horse ->
            blackPixel 'H'

        Cards.CardBack ->
            blackPixel '?'

        _ ->
            blackPixel ' '


cornerToPixel : Cards.LandscapeItem -> Pixel
cornerToPixel item =
    case item of
        Cards.Neutral ->
            blackPixel '.'

        Cards.Tournament ->
            { char = 'T', color = Color.purple }

        _ ->
            blackPixel '#'


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


edgeToPixel : Bool -> Cards.LandscapeItem -> Pixel
edgeToPixel mounting item =
    case item of
        Cards.Neutral ->
            case mounting of
                True ->
                    blackPixel '/'

                False ->
                    blackPixel '\\'

        Cards.Cross color ->
            { char = colorToChar color
            , color = Cards.colorToElmColor color
            }

        _ ->
            blackPixel '#'


flatEdgeToPixel : Cards.LandscapeItem -> Pixel
flatEdgeToPixel item =
    case item of
        Cards.Neutral ->
            blackPixel '-'

        Cards.Cross color ->
            { char = colorToChar color
            , color = Cards.colorToElmColor color
            }

        _ ->
            blackPixel '#'


pokeLeftCell : ( Int, Int ) -> LandscapeCard -> PixelMap -> PixelMap
pokeLeftCell ( row, col ) card pixelMap =
    let
        ( corner0, corner1, corner2 ) =
            card.corners

        ( edge0, edge1, edge2 ) =
            card.edges
    in
        pixelMap
            |> pokePixel ( row, col + 4 ) (cornerToPixel corner0)
            |> pokePixel ( row + 4, col + 8 ) (cornerToPixel corner1)
            |> pokePixel ( row + 4, col ) (cornerToPixel corner2)
            |> pokeMonoPixel ( row + 1, col + 5 ) '\\'
            |> pokePixel ( row + 2, col + 6 ) (edgeToPixel False edge0)
            |> pokeMonoPixel ( row + 3, col + 7 ) '\\'
            |> pokeMonoPixel ( row + 1, col + 3 ) '/'
            |> pokePixel ( row + 2, col + 2 ) (edgeToPixel True edge2)
            |> pokeMonoPixel ( row + 3, col + 1 ) '/'
            |> pokeMonoPixel ( row + 4, col + 1 ) '-'
            |> pokeMonoPixel ( row + 4, col + 2 ) '-'
            |> pokeMonoPixel ( row + 4, col + 3 ) '-'
            |> pokePixel ( row + 4, col + 4 ) (flatEdgeToPixel edge1)
            |> pokeMonoPixel ( row + 4, col + 5 ) '-'
            |> pokeMonoPixel ( row + 4, col + 6 ) '-'
            |> pokeMonoPixel ( row + 4, col + 7 ) '-'
            |> pokePixel ( row + 2, col + 4 ) (landscapeToPixel card.center)
            |> pokeMonoPixel ( row + 1, col + 4 ) ' '
            |> pokeMonoPixel ( row + 2, col + 3 ) ' '
            |> pokeMonoPixel ( row + 2, col + 5 ) ' '
            |> pokeMonoPixel ( row + 3, col + 2 ) ' '
            |> pokeMonoPixel ( row + 3, col + 3 ) ' '
            |> pokeMonoPixel ( row + 3, col + 4 ) ' '
            |> pokeMonoPixel ( row + 3, col + 5 ) ' '
            |> pokeMonoPixel ( row + 3, col + 6 ) ' '


pixelizeLeftCell : ( Int, Int ) -> LandscapeCard -> PixelMap -> PixelMap
pixelizeLeftCell ( x, y ) card pixelMap =
    let
        row =
            y * 4

        col =
            x * 8 - (y * 4)
    in
        pokeLeftCell ( row, col ) card pixelMap


pokeRightCell : ( Int, Int ) -> LandscapeCard -> PixelMap -> PixelMap
pokeRightCell ( row, col ) card pixelMap =
    let
        ( corner0, corner1, corner2 ) =
            card.corners

        ( edge0, edge1, edge2 ) =
            card.edges
    in
        pixelMap
            |> pokePixel ( row, col + 8 ) (cornerToPixel corner0)
            |> pokePixel ( row + 4, col + 4 ) (cornerToPixel corner1)
            |> pokePixel ( row, col ) (cornerToPixel corner2)
            |> pokeMonoPixel ( row + 1, col + 7 ) '/'
            |> pokePixel ( row + 2, col + 6 ) (edgeToPixel True edge0)
            |> pokeMonoPixel ( row + 3, col + 5 ) '/'
            |> pokeMonoPixel ( row + 3, col + 3 ) '\\'
            |> pokePixel ( row + 2, col + 2 ) (edgeToPixel False edge1)
            |> pokeMonoPixel ( row + 1, col + 1 ) '\\'
            |> pokeMonoPixel ( row, col + 1 ) '-'
            |> pokeMonoPixel ( row, col + 2 ) '-'
            |> pokeMonoPixel ( row, col + 3 ) '-'
            |> pokePixel ( row, col + 4 ) (flatEdgeToPixel edge2)
            |> pokeMonoPixel ( row, col + 5 ) '-'
            |> pokeMonoPixel ( row, col + 6 ) '-'
            |> pokeMonoPixel ( row, col + 7 ) '-'
            |> pokePixel ( row + 2, col + 4 ) (landscapeToPixel card.center)
            |> pokeMonoPixel ( row + 1, col + 2 ) ' '
            |> pokeMonoPixel ( row + 1, col + 3 ) ' '
            |> pokeMonoPixel ( row + 1, col + 4 ) ' '
            |> pokeMonoPixel ( row + 1, col + 5 ) ' '
            |> pokeMonoPixel ( row + 1, col + 6 ) ' '
            |> pokeMonoPixel ( row + 2, col + 3 ) ' '
            |> pokeMonoPixel ( row + 2, col + 5 ) ' '
            |> pokeMonoPixel ( row + 3, col + 4 ) ' '


pixelizeRightCell : ( Int, Int ) -> LandscapeCard -> PixelMap -> PixelMap
pixelizeRightCell ( x, y ) card pixelMap =
    let
        row =
            y * 4

        col =
            x * 8 - (y * 4) + 4
    in
        pokeRightCell ( row, col ) card pixelMap


pixelizeCell : Board.CellCoordinates -> LandscapeCard -> PixelMap -> PixelMap
pixelizeCell ( x, y, position ) card pixelMap =
    case position of
        Board.CellLeft ->
            pixelizeLeftCell ( x, y ) card pixelMap

        Board.CellRight ->
            pixelizeRightCell ( x, y ) card pixelMap


maybePixelizeCell : Board.CellCoordinates -> Maybe LandscapeCard -> PixelMap -> PixelMap
maybePixelizeCell coord maybeCard pixelMap =
    case maybeCard of
        Nothing ->
            pixelMap

        Just card ->
            pixelizeCell coord card pixelMap


pixelizeBoard : Board.Board -> PixelMap -> PixelMap
pixelizeBoard board pixelMap =
    let
        pixelizeBinome x y cellBinome pixelMap =
            pixelMap
                |> maybePixelizeCell ( x, y, Board.CellLeft ) cellBinome.left
                |> maybePixelizeCell ( x, y, Board.CellRight ) cellBinome.right

        pixelizeColumn : Int -> Board.Column Cards.LandscapeCard -> PixelMap -> PixelMap
        pixelizeColumn x column pixelMap =
            Dict.foldl (pixelizeBinome x) pixelMap column
    in
        Dict.foldl pixelizeColumn pixelMap board.landscapes


grayIt : PixelMap -> PixelMap
grayIt pixelMap =
    let
        grayCell _ cell =
            { char = cell.char, color = Color.rgb 220 220 220 }

        grayColumn _ column =
            Dict.map grayCell column
    in
        Dict.map grayColumn pixelMap
