module Main (..) where

import Debug
import Dict
import Result
import Html exposing (div, button, text, pre, span)
import Graphics.Element exposing (..)
import Html.Events exposing (onClick, onMouseDown, onMouseUp, onMouseMove, on)
import Html.Attributes exposing (..)
import Color
import Text
import Keyboard
import Mouse
import Time
import StartApp
import Effects exposing (Never)
import Cards exposing (..)
import Board exposing (setCell, Board)
import Rules
import Render
import Json.Decode as Decode exposing (Decoder, (:=))


type alias Model =
  { board : Board
  , topLeft : ( Int, Int )
  , mousePressed : Bool
  , mousePressedInitialPos : ( Int, Int )
  , mouseCurrentPos : ( Int, Int )
  , mouseCurrentCharPos : ( Int, Int )
  , charSize : ( Float, Float )
  }


tmpInitBoard : Board -> Board
tmpInitBoard board =
  board
    |> Board.setLandscape ( 2, 0, Board.CellLeft ) (getLandscapeCardAndRotate 3 0)
    |> Board.setLandscape ( 2, 0, Board.CellRight ) (getLandscapeCardAndRotate 3 0)
    |> Board.setLandscape ( 7, 9, Board.CellRight ) (getLandscapeCardAndRotate 15 1)
    |> Board.setLandscape ( 15, 15, Board.CellLeft ) (getLandscapeCardAndRotate 7 2)


init =
  ( { board = Board.init Board.CellLeft |> tmpInitBoard
    , topLeft = ( 0, 0 )
    , mousePressed = False
    , mousePressedInitialPos = ( 0, 0 )
    , mousePressedInitialBoard = ( 0, 0 )
    , mouseCurrentPos = ( 0, 0 )
    , mouseCurrentCharPos = ( 0, 0 )
    , charSize = ( 7.22, 14.0 )
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
      , ( "font-family", "monospace,monospace" )
      , ( "font-size", "12px" )
      , ( "flex", "1" )
      , ( "position", "relative" )
      , ( "background-color", backgroundColor )
      , ( "-webkit-user-select", "none" )
      , ( "text-rendering", "optimizeLegibility" )
        --      , ( "cursor", "move" )
      ]


dashboardStyle : Html.Attribute
dashboardStyle =
  style
    [ ( "font-size", "1em" )
    , ( "font-family", "monospace,monospace" )
    , ( "background-color", "#E0E0E0" )
    , ( "-webkit-user-select", "none" )
    , ( "width", "7em" )
    ]


redSquareStyle : Html.Attribute
redSquareStyle =
  style
    [ ( "background-color", "red" )
    , ( "position", "absolute" )
    , ( "top", "0" )
    , ( "left", "0" )
    , ( "width", "101" )
    , ( "height", "140" )
    ]


leftItems : List Html.Html
leftItems =
  Dict.empty
    |> Render.pokeLeftCell ( 0, 1 ) backCard
    |> Render.pokeLeftCell ( 1, 1 ) backCard
    |> Render.pokeLeftCell ( 2, 1 ) backCard
    |> Render.renderMapToHtml ( 0, 0 )


decoderPos : Decoder ( Int, Int )
decoderPos =
  Decode.object2
    (,)
    ("clientX" := Decode.int)
    ("clientY" := Decode.int)


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
        , on "mousemove" decoderPos (\pos -> Signal.message address (MouseMove pos))
        , landscapeStyle model.mousePressed
        ]
        ((div
            [ redSquareStyle ]
            []
         )
          :: (model.board
                |> Render.render
                |> Render.pokePixel model.mouseCurrentCharPos { char = '@', color = Color.red }
                |> Render.renderMapToHtml model.topLeft
             )
        )
    ]


type Action
  = Move Int Int
  | MousePress Bool
  | MouseMove ( Int, Int )
  | CharSizeResult ( Float, Float )
  | NoOp


mouseMoveWhilePressed ( x, y ) model =
  let
    model' =
      { model | mouseCurrentPos = ( x, y ) }

    ( initialX, initialY ) =
      model.mousePressedInitialPos

    deltaX =
      (x - initialX) // 8

    deltaY =
      (y - initialY) // 14
  in
    { model'
      | topLeft =
          ( fst model.mousePressedInitialBoard - deltaX
          , snd model.mousePressedInitialBoard - deltaY
          )
    }


mouseMove ( x, y ) model =
  let
    ( charWidth, charHeight ) =
      model.charSize
  in
    if model.mousePressed then
      mouseMoveWhilePressed ( x, y ) model
    else
      { model
        | mouseCurrentPos = ( x, y )
        , mouseCurrentCharPos = ( (round ((toFloat y) / charHeight)) + snd model.topLeft, (round ((toFloat x) / charWidth)) + fst model.topLeft )
      }


update action model =
  case action of
    NoOp ->
      ( model, Effects.none )

    CharSizeResult size ->
      ( { model
          | charSize = (Debug.log "size" size)
        }
      , Effects.none
      )

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


port requestCharSize : Signal ( String, Int )
port requestCharSize =
  Signal.map (\_ -> ( "monospace", 12 )) (Time.every Time.second)


port charSizeResult : Signal ( Float, Float )
charSizeResultAction : Signal Action
charSizeResultAction =
  Signal.map CharSizeResult charSizeResult


app =
  StartApp.start
    { init = init
    , view = view
    , update = update
    , inputs =
        [ Signal.map
            spaceToInc
            Keyboard.wasd
        , charSizeResultAction
          -- , Signal.map
          --     mouseMoveToAction
          --     Mouse.position
        ]
    }


main =
  app.html
