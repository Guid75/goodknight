port module GoodKnight exposing (..)

import Debug
import Dict
import Result
import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Color
import Keyboard
import Mouse
import Time
import Cards exposing (..)
import Board exposing (setCell, Board, CellCoordinates, CellPosition(..))
import Rules
import Render


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { board : Board
    , topLeft : ( Int, Int )
    , mousePressed : Bool
    , mousePressedInitialBoard : ( Int, Int )
    , mousePressedInitialPos : ( Int, Int )
    , mouseCurrentPos : ( Int, Int )
    , landscapeMousePos : ( Int, Int )
    , landscapeFontName : String
    , landscapeFontSize : Int
    , landscapeCharSize : FloatSize
    }


type alias FloatSize =
    { w : Float
    , h : Float
    }


tmpInitBoard : Board -> Board
tmpInitBoard board =
    board
        |> Board.setLandscape ( 2, 0, Board.CellLeft ) (getLandscapeCardAndRotate 3 0)
        |> Board.setLandscape ( 2, 0, Board.CellRight ) (getLandscapeCardAndRotate 3 0)
        |> Board.setLandscape ( 7, 9, Board.CellRight ) (getLandscapeCardAndRotate 15 1)
        |> Board.setLandscape ( 15, 15, Board.CellLeft ) (getLandscapeCardAndRotate 7 2)


defaultLandscapeFontName : String
defaultLandscapeFontName =
    "monospace"


defaultLandscapeFontSize : Int
defaultLandscapeFontSize =
    24


init : ( Model, Cmd Msg )
init =
    ( { board = Board.init Board.CellLeft |> tmpInitBoard
      , topLeft = ( 0, 0 )
      , mousePressed = False
      , mousePressedInitialPos = ( 0, 0 )
      , mousePressedInitialBoard = ( 0, 0 )
      , mouseCurrentPos = ( 0, 0 )
      , landscapeFontName = defaultLandscapeFontName
      , landscapeFontSize = defaultLandscapeFontSize
      , landscapeMousePos = ( 0, 0 )
      , landscapeCharSize = { w = 7.50, h = 14.0 }
      }
    , requestCharSize ( defaultLandscapeFontName, defaultLandscapeFontSize )
    )


landscapeStyle : Model -> Html.Attribute msg
landscapeStyle model =
    let
        backgroundColor =
            if model.mousePressed then
                "#FFE0E0"
            else
                "#FFFFFF"
    in
        style
            [ ( "overflow", "hidden" )
            , ( "font-family", model.landscapeFontName )
            , ( "font-size", (toString model.landscapeFontSize) ++ "px" )
            , ( "flex", "1" )
            , ( "position", "relative" )
            , ( "background-color", backgroundColor )
            , ( "-webkit-user-select", "none" )
            , ( "text-rendering", "optimizeLegibility" )
            , ( "cursor", "grab" )
            , ( "margin", "0px" )
            ]


dashboardStyle : Html.Attribute msg
dashboardStyle =
    style
        [ ( "font-size", "1em" )
        , ( "font-family", "monospace,monospace" )
        , ( "background-color", "#E0E0E0" )
        , ( "-webkit-user-select", "none" )
        , ( "width", "7em" )
        , ( "margin", "0px" )
        ]


leftItems : List (Html msg)
leftItems =
    Dict.empty
        |> Render.pokeLeftCell ( 0, 1 ) backCard
        |> Render.pokeLeftCell ( 1, 1 ) backCard
        |> Render.pokeLeftCell ( 2, 1 ) backCard
        |> Render.renderMapToHtml ( 0, 0 )


view : Model -> Html Msg
view model =
    let
        mouseCurrentCharPos =
            mousePosToCharPos model.landscapeCharSize model.topLeft model.mouseCurrentPos
    in
        div
            [ style
                [ ( "overflow", "hidden" )
                , ( "display", "flex" )
                , ( "flex-direction", "row" )
                ]
            ]
            [ pre [ dashboardStyle ]
                leftItems
            , pre
                [ onMouseDown (MousePress True)
                , onMouseUp (MousePress False)
                , landscapeStyle model
                , id "landscape"
                ]
                (model.board
                    |> Render.render
                    |> (if model.mousePressed then
                            identity
                        else
                            Render.pokePixel mouseCurrentCharPos { char = '@', color = Color.red }
                       )
                    |> Render.renderMapToHtml model.topLeft
                )
            ]


type Msg
    = Move Int Int
    | MousePress Bool
    | MouseMove { x : Int, y : Int }
    | CharSizeResult ( Float, Float )
    | LandscapeMousePos ( Int, Int )
    | RequestCharSize
    | NoOp


mouseMoveWhilePressed : Model -> Model
mouseMoveWhilePressed model =
    let
        ( x, y ) =
            model.mouseCurrentPos

        ( initialX, initialY ) =
            model.mousePressedInitialPos

        deltaX =
            floor ((toFloat (x - initialX)) / model.landscapeCharSize.w)

        deltaY =
            floor ((toFloat (y - initialY)) / model.landscapeCharSize.h)
    in
        { model
            | topLeft =
                ( fst model.mousePressedInitialBoard - deltaX
                , snd model.mousePressedInitialBoard - deltaY
                )
        }


mousePosToCharPos : FloatSize -> ( Int, Int ) -> ( Int, Int ) -> ( Int, Int )
mousePosToCharPos charSize ( left, top ) ( x, y ) =
    ( (floor ((toFloat y) / charSize.h)) + top
    , (floor ((toFloat x) / charSize.w)) + left
    )


mouseMove : ( Int, Int ) -> Model -> Model
mouseMove ( x, y ) model =
    let
        model' =
            { model | mouseCurrentPos = ( x, y ) }
    in
        if model.mousePressed then
            mouseMoveWhilePressed model'
        else
            model'


getHoveredCell : Model -> CellCoordinates
getHoveredCell model =
    let
        absoluteMouseCoord =
            -- use floor instead of round?
            ( fst model.mouseCurrentPos + round ((toFloat (fst model.topLeft)) * model.landscapeCharSize.w)
            , snd model.mouseCurrentPos + round ((toFloat (snd model.topLeft)) * model.landscapeCharSize.h)
            )
    in
        ( 0, 0, CellLeft )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        RequestCharSize ->
            ( model, requestCharSize ( model.landscapeFontName, model.landscapeFontSize ) )

        CharSizeResult size ->
            { model | landscapeCharSize = { w = fst size, h = snd size } } ! []

        LandscapeMousePos pos ->
            mouseMove pos model ! []

        Move x y ->
            { model
                | topLeft = ( fst model.topLeft - x, snd model.topLeft + y )
            }
                ! []

        MouseMove pos ->
            ( model, requestLandscapeMousePos ( pos.x, pos.y ) )

        MousePress pressed ->
            { model
                | mousePressed = pressed
                , mousePressedInitialPos = model.mouseCurrentPos
                , mousePressedInitialBoard = model.topLeft
            }
                ! []


spaceToInc : { x : Int, y : Int } -> Msg
spaceToInc { x, y } =
    Move x y


port requestCharSize : ( String, Int ) -> Cmd msg


port charSizeResult : (( Float, Float ) -> msg) -> Sub msg


port requestLandscapeMousePos : ( Int, Int ) -> Cmd msg


port landscapeMousePosResult : (( Int, Int ) -> msg) -> Sub msg



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ charSizeResult CharSizeResult
        , Mouse.moves MouseMove
        , landscapeMousePosResult LandscapeMousePos
        ]
