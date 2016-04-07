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
import Board exposing (setCell, Board)
import Rules
import Render


type alias Model =
    { board : Board
    , topLeft : ( Int, Int )
    , mousePressed : Bool
    , mousePressedInitialPos : ( Int, Int )
    , mouseCurrentPos : ( Int, Int )
    , board : Board
    }



-- b = Debug.log "ok" (Rules.isPossibleMove
--     (0, -1, Board.CellRight)
--     2  3  board)
-- b3 = Debug.log "ok3" (Rules.isPossibleMove
--                       (0, 1, Board.CellRight)
--                       2  0  board)
-- rotatedCard = Debug.log "ok" (Maybe.map (rotateLandscapeCard 0) (Array.get 0 initialLandscapeDeck))
{-
Init some random cards into a board
-}


tmpInitBoard : Board -> Board
tmpInitBoard board =
    board
        |> Board.setLandscape ( 1, 0, Board.CellLeft ) (getLandscapeCardAndRotate 3 0)
        |> Board.setLandscape ( 0, 0, Board.CellRight ) (getLandscapeCardAndRotate 3 0)
        |> Board.setLandscape ( 7, 9, Board.CellRight ) (getLandscapeCardAndRotate 15 1)
        |> Board.setLandscape ( 15, 15, Board.CellLeft ) (getLandscapeCardAndRotate 7 2)


init =
    ( { board = Board.init Board.CellLeft |> tmpInitBoard
      , topLeft = ( 0, 0 )
      , mousePressed = False
      , mousePressedInitialPos = ( 0, 0 )
      , mousePressedInitialBoard = ( 0, 0 )
      , mouseCurrentPos = ( 0, 0 )
      }
    , Effects.none
    )


landscapeStyle : Html.Attribute
landscapeStyle =
    style
        [ ( "font-size", "15px" )
        , ( "font-family", "monospace" )
        , ( "text-align", "center" )
        ]


dashboardStyle : Html.Attribute
dashboardStyle =
    style
        [ ( "font-size", "12px" )
        , ( "font-family", "monospace" )
        ]


view address model =
    div
        [ style
            [ ( "overflow", "hidden" )
            , ( "display", "flex" )
            , ( "flex-direction", "row" )
            ]
        ]
        [ pre
            [ dashboardStyle ]
            [ Render.pokeLeftCell ( 0, 0 ) neutralCard Dict.empty
                |> Render.pokeLeftCell ( 2, 0 ) neutralCard
                |> Render.pokeLeftCell ( 4, 0 ) neutralCard
                |> Render.renderMapToText ( 0, 0 )
                |> text
            ]
        , pre
            [ style
                [ ( "overflow", "hidden" )
                , ( "font-size", "12px" )
                , ( "flex", "1" )
                ]
            ]
            [ model.board
                |> Render.render
                |> Render.renderMapToText model.topLeft
                |> text
            ]
        ]



-- div
--     []
--     (List.map (textToDiv landscapeStyle) (Array.toList initialLandscapeDeck))
{- transform anything into an HTML div with just its textual representation -}


textToDiv : Html.Attribute -> a -> Html.Html
textToDiv attr a =
    div [ attr ] [ (text << toString) a ]


type Action
    = Move Int Int
    | MousePress Bool
    | MouseMove ( Int, Int )
    | NoOp


mouseMoveWhilePressed ( x, y ) model =
    let
        model' = { model | mouseCurrentPos = ( x, y ) }

        initialPos = model.mousePressedInitialPos

        deltaX = (x - fst initialPos) // 12

        deltaY = (y - snd initialPos) // 12
    in
        { model'
            | topLeft =
                ( fst model.mousePressedInitialBoard - deltaX
                , snd model.mousePressedInitialBoard - deltaY
                )
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
                | topLeft = ( fst model.topLeft - x, snd model.topLeft + y )
              }
            , Effects.none
            )

        MouseMove pos ->
            ( mouseMove pos model, Effects.none )

        MousePress pressed ->
            ( { model
                | mousePressed = pressed
                , mousePressedInitialPos = model.mouseCurrentPos
                , mousePressedInitialBoard = model.topLeft
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
