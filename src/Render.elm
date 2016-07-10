module Render exposing (renderMapToHtml)

import Cards
import Board
import List
import String
import Dict exposing (Dict)
import Color exposing (Color)
import Html exposing (Html, text, span, pre)
import Html.Attributes exposing (..)
import PixelMap exposing (PixelMap, PixelRow, Pixel)


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


renderRow : Int -> PixelRow -> List (Html msg)
renderRow left row =
    let
        ( _, renderItems ) =
            Dict.filter (\colIndex _ -> colIndex >= left) row
                |> Dict.foldl renderPixelsSince ( left - 1, [] )
    in
        List.concat [ renderItems, [ crHtml ] ]


renderRowsSince : Int -> Int -> PixelRow -> ( Int, List (Html msg) ) -> ( Int, List (Html msg) )
renderRowsSince left rowIndex row ( oldRowIndex, renderItems ) =
    let
        emptyLinesCount =
            rowIndex - oldRowIndex - 1

        emptyLines =
            List.repeat emptyLinesCount crHtml
    in
        ( rowIndex, List.concat [ renderItems, emptyLines, (renderRow left row) ] )


renderMapToHtml : ( Int, Int ) -> PixelMap -> List (Html msg)
renderMapToHtml ( left, top ) renderMap =
    let
        ( oldRowIndex, renderItems ) =
            Dict.filter (\rowIndex _ -> rowIndex >= top) renderMap
                |> Dict.foldl (renderRowsSince left) ( top - 1, [] )
    in
        renderItems
