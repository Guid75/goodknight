module Main (..) where

import Debug
import Dict
import Result
import Html exposing (div, button, text, pre, span)
import Graphics.Element exposing (..)
import Html.Events exposing (onClick, onMouseDown, onMouseUp)
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
    }


tmpInitBoard : Board -> Board
tmpInitBoard board =
    board
        |> Board.setLandscape ( 1, 0, Board.CellLeft ) (getLandscapeCardAndRotate 3 0)
        |> Board.setLandscape ( 1, 0, Board.CellRight ) (getLandscapeCardAndRotate 3 0)
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


landscapeStyle : Bool -> Html.Attribute
landscapeStyle mousePressed =
    let
        backgroundColor =
            if mousePressed then
                "#FFE0E0"
            else
                "#FFFFFF"
    in
        style
            [ ( "overflow", "hidden" )
            , ( "font-size", "12px" )
            , ( "flex", "1" )
            , ( "background-color", backgroundColor )
            , ( "-webkit-user-select", "none" )
            , ( "cursor", "move" )
            ]


dashboardStyle : Html.Attribute
dashboardStyle =
    style
        [ ( "font-size", "12px" )
        , ( "font-family", "monospace" )
        , ( "background-color", "#E0E0E0" )
        , ( "-webkit-user-select", "none" )
        , ( "width", "7em" )
        ]


leftItems : List Html.Html
leftItems =
    Dict.empty
        |> Render.pokeLeftCell ( 0, 1 ) backCard
        |> Render.pokeLeftCell ( 1, 1 ) backCard
        |> Render.pokeLeftCell ( 2, 1 ) backCard
        |> Render.renderMapToHtml ( 0, 0 )


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
            leftItems
        , pre
            [ onMouseDown address (MousePress True)
            , onMouseUp address (MousePress False)
            , landscapeStyle model.mousePressed
            ]
            (model.board
              |> Render.render
              |> Render.renderMapToHtml model.topLeft)
        ]


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


mouseMoveToAction : ( Int, Int ) -> Action
mouseMoveToAction pos =
    MouseMove pos


port requestOffset : Signal ( Int, Int )
port requestOffset =
    Mouse.position


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
            ]
        }


main =
    app.html
