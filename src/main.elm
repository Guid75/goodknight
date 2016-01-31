module Main (..) where

import Debug
import Html exposing (div, button, text)
import Graphics.Element exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)
import Text
import StartApp.Simple as StartApp
import Cards exposing (..)
import Board exposing (setCell)
import Rules
import Dict
import Array
import Maybe
import Result


main =
    StartApp.start { model = model, view = view, update = update }


model =
    0


board =
    Board.init



-- b = Debug.log "ok" (Rules.isPossibleMove
--     (0, -1, Board.CellRight)
--     2  3  board)
-- b3 = Debug.log "ok3" (Rules.isPossibleMove
--                       (0, 1, Board.CellRight)
--                       2  0  board)
-- rotatedCard = Debug.log "ok" (Maybe.map (rotateLandscapeCard 0) (Array.get 0 initialLandscapeDeck))


newBoard =
    Debug.log
        "new board:"
        (board
            |> Board.setLandscape ( 0, 1, Board.CellLeft ) (getLandscapeCardAndRotate 10 0)
            |> Board.setLandscape ( 7, 9, Board.CellLeft ) (getLandscapeCardAndRotate 15 1)
            |> Board.setLandscape ( 0, 15, Board.CellLeft ) (getLandscapeCardAndRotate 7 2)
        )


-- b2 =
--     Debug.log
--         "ok2"
--         (Rules.isPossibleMove
--             ( 0, 0, Board.CellRight )
--             2
--             0
--             newBoard
--         )


view address model =
    div
        []
        (List.map (textToDiv landscapeStyle) (Array.toList initialLandscapeDeck))



-- transform anything into an HTML div with just its textual representation


textToDiv : Html.Attribute -> a -> Html.Html
textToDiv attr a =
    div [ attr ] [ (text << toString) a ]


landscapeStyle : Html.Attribute
landscapeStyle =
    style
        [ ( "font-size", "15px" )
        , ( "font-family", "monospace" )
        , ( "text-align", "center" )
        ]


type Action
    = Increment
    | Decrement


update action model =
    case action of
        Increment ->
            model + 1

        Decrement ->
            model - 1
