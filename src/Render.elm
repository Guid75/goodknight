module Render exposing (render, renderMapToHtml, pokeLeftCell, pokePixel, pokeRightCell)

import Cards
import Board
import List
import String
import Dict exposing (Dict)
import Color exposing (Color)
import Html exposing (Html, text, span, pre)
import Html.Attributes exposing (..)


-- indexed by rows, then by columns


type alias Pixel =
  { char : Char
  , color : Color
  }


blackPixel : Char -> Pixel
blackPixel c =
  { char = c, color = Color.black }


type alias RenderRow =
  Dict Int Pixel


type alias RenderMap =
  Dict Int RenderRow


pokePixel : ( Int, Int ) -> Pixel -> RenderMap -> RenderMap
pokePixel ( row, col ) pixel renderMap =
  let
    maybeRow =
      Dict.get row renderMap
  in
    case maybeRow of
      Just rowMap ->
        Dict.insert row (Dict.insert col pixel rowMap) renderMap

      Nothing ->
        Dict.insert row (Dict.singleton col pixel) renderMap


pokeMonoPixel : ( Int, Int ) -> Char -> RenderMap -> RenderMap
pokeMonoPixel pos c renderMap =
  pokePixel pos (blackPixel c) renderMap


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


pokeLeftCell : ( Int, Int ) -> Cards.LandscapeCard -> RenderMap -> RenderMap
pokeLeftCell ( row, col ) card renderMap =
  let
    ( corner0, corner1, corner2 ) =
      card.corners

    ( edge0, edge1, edge2 ) =
      card.edges
  in
    renderMap
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


renderLeftCell : ( Int, Int ) -> Maybe Cards.LandscapeCard -> RenderMap -> RenderMap
renderLeftCell ( x, y ) maybeCard renderMap =
  case maybeCard of
    Nothing ->
      renderMap

    Just card ->
      let
        row =
          y * 4

        col =
          x * 8 - (y * 4)
      in
        pokeLeftCell ( row, col ) card renderMap


pokeRightCell : ( Int, Int ) -> Cards.LandscapeCard -> RenderMap -> RenderMap
pokeRightCell ( row, col ) card renderMap =
  let
    ( corner0, corner1, corner2 ) =
      card.corners

    ( edge0, edge1, edge2 ) =
      card.edges
  in
    renderMap
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


renderRightCell : ( Int, Int ) -> Maybe Cards.LandscapeCard -> RenderMap -> RenderMap
renderRightCell ( x, y ) maybeCard renderMap =
  case maybeCard of
    Nothing ->
      renderMap

    Just card ->
      let
        row =
          y * 4

        col =
          x * 8 - (y * 4) + 4
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


crHtml : Html msg
crHtml =
  text "\n"


colorToCss : Color -> String
colorToCss color =
  let
    rgb =
      Color.toRgb color
  in
    "rgb("
      ++ (toString rgb.red)
      ++ ","
      ++ (toString rgb.green)
      ++ ","
      ++ (toString rgb.blue)
      ++ ")"


pixelToSpan : Pixel -> Html msg
pixelToSpan pixel =
  span
    [ style
        [ ( "color", colorToCss pixel.color )
        ]
    ]
    [ text (String.fromChar pixel.char) ]


renderPixelsSince : Int -> Pixel -> ( Int, List (Html msg) ) -> ( Int, List (Html msg) )
renderPixelsSince colIndex pixel ( oldColIndex, renderItems ) =
  let
    emptyPixelsCount =
      colIndex - oldColIndex - 1

    emptyPixels =
      text (String.repeat emptyPixelsCount " ")
  in
    ( colIndex
    , List.concat
        [ renderItems
        , [ emptyPixels ]
        , [ pixelToSpan pixel ]
        ]
    )


renderRow : Int -> RenderRow -> List (Html msg)
renderRow left row =
  let
    ( _, renderItems ) =
      Dict.filter (\colIndex _ -> colIndex >= left) row
        |> Dict.foldl renderPixelsSince ( left - 1, [] )
  in
    List.concat [ renderItems, [ crHtml ] ]


renderRowsSince : Int -> Int -> RenderRow -> ( Int, List (Html msg) ) -> ( Int, List (Html msg) )
renderRowsSince left rowIndex row ( oldRowIndex, renderItems ) =
  let
    emptyLinesCount =
      rowIndex - oldRowIndex - 1

    emptyLines =
      List.repeat emptyLinesCount crHtml
  in
    ( rowIndex, List.concat [ renderItems, emptyLines, (renderRow left row) ] )


renderMapToHtml : ( Int, Int ) -> RenderMap -> List (Html msg)
renderMapToHtml ( left, top ) renderMap =
  let
    ( oldRowIndex, renderItems ) =
      Dict.filter (\rowIndex _ -> rowIndex >= top) renderMap
        |> Dict.foldl (renderRowsSince left) ( top - 1, [] )
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
