module PixelMap
    exposing
        ( PixelMap
        , PixelRow
        , Pixel
        , render
        , renderCell
        , pokeLeftCell
        , pokePixel
        , pokeRightCell
        , grayIt
        )

import Cards
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


pokeLeftCell : ( Int, Int ) -> Cards.LandscapeCard -> PixelMap -> PixelMap
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


renderLeftCell : ( Int, Int ) -> Maybe Cards.LandscapeCard -> PixelMap -> PixelMap
renderLeftCell ( x, y ) maybeCard pixelMap =
    case maybeCard of
        Nothing ->
            pixelMap

        Just card ->
            let
                row =
                    y * 4

                col =
                    x * 8 - (y * 4)
            in
                pokeLeftCell ( row, col ) card pixelMap


pokeRightCell : ( Int, Int ) -> Cards.LandscapeCard -> PixelMap -> PixelMap
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


renderRightCell : ( Int, Int ) -> Maybe Cards.LandscapeCard -> PixelMap -> PixelMap
renderRightCell ( x, y ) maybeCard pixelMap =
    case maybeCard of
        Nothing ->
            pixelMap

        Just card ->
            let
                row =
                    y * 4

                col =
                    x * 8 - (y * 4) + 4
            in
                pokeRightCell ( row, col ) card pixelMap


renderCell : Board.CellCoordinates -> Board.CellBinome Cards.LandscapeCard -> PixelMap -> PixelMap
renderCell ( x, y, position ) cellBinome pixelMap =
    case position of
        Board.CellLeft ->
            renderLeftCell ( x, y ) cellBinome.left pixelMap

        Board.CellRight ->
            renderRightCell ( x, y ) cellBinome.right pixelMap


render : Board.Board -> PixelMap -> PixelMap
render board pixelMap =
    let
        renderBinome x y cellBinome pixelMap =
            renderCell ( x, y, Board.CellLeft ) cellBinome pixelMap
                |> renderCell ( x, y, Board.CellRight ) cellBinome

        renderColumn : Int -> Board.Column Cards.LandscapeCard -> PixelMap -> PixelMap
        renderColumn x column pixelMap =
            Dict.foldl (renderBinome x) pixelMap column
    in
        Dict.foldl renderColumn pixelMap board.landscapes


grayIt : PixelMap -> PixelMap
grayIt pixelMap =
    let
        grayCell _ cell =
            { char = cell.char, color = Color.rgb 220 220 220 }

        grayColumn _ column =
            Dict.map grayCell column
    in
        Dict.map grayColumn pixelMap
