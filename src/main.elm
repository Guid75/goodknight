module Main (..) where

import Debug
import Dict
import Array
import Maybe
import Result
import Html exposing (div, button, text, pre)
import Graphics.Element exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)
import Text
import Keyboard
import Mouse
import StartApp
import Effects exposing (Never)
import Cards exposing (..)
import Board exposing (setCell)
import Rules
import Render


type alias Model =
    { boardX : Int
    , boardY : Int
    , mousePressed : Bool
    , mousePressedInitialPos : ( Int, Int )
    , mouseCurrentPos : ( Int, Int )
    }


init =
    ( { boardX = 0
      , boardY = 0
      , mousePressed = False
      , mousePressedInitialPos = ( 0, 0 )
      , mousePressedInitialBoard = ( 0, 0 )
      , mouseCurrentPos = ( 0, 0 )
      }
    , Effects.none
    )


board =
    Board.init Board.CellLeft



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
            |> Board.setLandscape ( 1, 0, Board.CellLeft ) (getLandscapeCardAndRotate 3 0)
            |> Board.setLandscape ( 0, 0, Board.CellRight ) (getLandscapeCardAndRotate 3 0)
            |> Board.setLandscape ( 7, 9, Board.CellRight ) (getLandscapeCardAndRotate 15 1)
            |> Board.setLandscape ( 15, 15, Board.CellLeft ) (getLandscapeCardAndRotate 7 2)
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
--    .-------.
--   / \     /
--  R   Y   /
-- /     \ /
--T-------.-------.
-- \     / \     /
--  \   /   \   /
--   \ /     \ /
--    .-------.


renderMap =
    Render.render newBoard


renderMapText =
    Render.renderMapToText renderMap ( 0, 0 )


view address model =
    div
        [
         style
           [ ("overflow", "hidden")
           ]
        ]
        [ pre
            [
             style
               [ ("overflow", "hidden")
               ]
            ]
            [ text (Render.renderMapToText renderMap ( model.boardX, model.boardY )) ]
        ]



-- div
--     []
--     (List.map (textToDiv landscapeStyle) (Array.toList initialLandscapeDeck))
{- transform anything into an HTML div with just its textual representation -}


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
    = Move Int Int
    | MousePress Bool
    | MouseMove ( Int, Int )
    | NoOp


mouseMoveWhilePressed ( x, y ) model =
    let
        t = Debug.log "pouet" ( x, y )

        model' = { model | mouseCurrentPos = ( x, y ) }

        initialPos = model.mousePressedInitialPos

        deltaX = (x - fst initialPos) // 10

        deltaY = (y - snd initialPos) // 10
    in
        { model'
            | boardX = fst model.mousePressedInitialBoard - (Debug.log "deltaX" deltaX)
            , boardY = snd model.mousePressedInitialBoard - (Debug.log "deltaY" deltaY)
        }


mouseMove ( x, y ) model =
    if model.mousePressed then
        mouseMoveWhilePressed ( x, y ) model
    else
        { model | mouseCurrentPos = ( x, y ) }


update action model =
    case action of
        NoOp ->
            ( model, Effects.none )

        Move x y ->
            ( { model
                | boardX = model.boardX - x
                , boardY = model.boardY + y
              }
            , Effects.none
            )

        MouseMove pos ->
            ( mouseMove pos model, Effects.none )

        MousePress pressed ->
            ( { model
                | mousePressed = pressed
                , mousePressedInitialPos = model.mouseCurrentPos
                , mousePressedInitialBoard = ( model.boardX, model.boardY )
              }
            , Effects.none
            )


spaceToInc : { x : Int, y : Int } -> Action
spaceToInc { x, y } =
    Move x y


handleMouseDown : Bool -> Action
handleMouseDown isDown =
    MousePress isDown


mouseMoveToAction : ( Int, Int ) -> Action
mouseMoveToAction pos =
    MouseMove pos


app =
    StartApp.start
        { init = init
        , view = view
        , update = update
        , inputs =
            [ Signal.map
                spaceToInc
                Keyboard.wasd
            , Signal.map
                mouseMoveToAction
                Mouse.position
            , Signal.map
                handleMouseDown
                Mouse.isDown
            ]
        }


main =
    app.html
