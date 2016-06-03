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
    , hoveredCell : CellCoordinates
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
        |> Board.setLandscape ( 0, -1, Board.CellLeft ) (getLandscapeCardAndRotate 7 2)
        |> Board.setLandscape ( 0, -2, Board.CellLeft ) (getLandscapeCardAndRotate 7 2)
        |> Board.setLandscape ( 0, -3, Board.CellLeft ) (getLandscapeCardAndRotate 15 1)


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
      , landscapeCharSize = { w = 7.5, h = 14.0 }
      , hoveredCell = ( 0, 0, CellLeft )
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
                , ( "height", "100vh" )
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
                    |> Render.renderCell model.hoveredCell { left = Just backCard, right = Just backCard }
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


type alias Point =
    { x : Float
    , y : Float
    }


type alias Triangle =
    ( Point, Point, Point )


isEven : Int -> Bool
isEven n =
    n `rem` 2 == 0


isInTriangle : Triangle -> Point -> Bool
isInTriangle ( p1, p2, p3 ) { x, y } =
    let
        a =
            ((p2.y - p3.y) * (x - p3.x) + (p3.x - p2.x) * (y - p3.y))
                / ((p2.y - p3.y) * (p1.x - p3.x) + (p3.x - p2.x) * (p1.y - p3.y))

        b =
            ((p3.y - p1.y) * (x - p3.x) + (p1.x - p3.x) * (y - p3.y))
                / ((p2.y - p3.y) * (p1.x - p3.x) + (p3.x - p2.x) * (p1.y - p3.y))

        c =
            1 - a - b
    in
        0 <= a && a <= 1 && 0 <= b && b <= 1 && 0 <= c && c <= 1


triangleBoundingSize : FloatSize -> FloatSize
triangleBoundingSize { w, h } =
    { w = 8 * w
    , h = 4 * h
    }


getTriangle : FloatSize -> CellCoordinates -> Triangle
getTriangle landscapeCharSize ( col, row, cellPosition ) =
    let
        triangleSize =
            triangleBoundingSize { w = landscapeCharSize.w, h = landscapeCharSize.h }

        x =
            if isEven row then
                landscapeCharSize.w / 2.0 + (toFloat col) * triangleSize.w - triangleSize.w * toFloat (row // 2)
            else
                landscapeCharSize.w / 2.0 + (toFloat col) * triangleSize.w - triangleSize.w * toFloat ((row - 1) // 2) - triangleSize.w / 2.0

        y =
            triangleSize.h * toFloat row + landscapeCharSize.h / 2.0
    in
        case cellPosition of
            CellLeft ->
                ( { x = x + triangleSize.w / 2.0, y = y }, { x = x + triangleSize.w, y = y + triangleSize.h }, { x = x, y = y + triangleSize.h } )

            CellRight ->
                ( { x = x + triangleSize.w / 2.0, y = y }, { x = x + triangleSize.w * 1.5, y = y }, { x = x + triangleSize.w, y = y + triangleSize.h } )


getHoveredCell : Model -> CellCoordinates
getHoveredCell model =
    let
        absoluteMouseCoord : Point
        absoluteMouseCoord =
            { x = toFloat (fst model.mouseCurrentPos) + toFloat (fst model.topLeft) * model.landscapeCharSize.w
            , y = toFloat (snd model.mouseCurrentPos) + toFloat (snd model.topLeft) * model.landscapeCharSize.h
            }

        triangleSize =
            triangleBoundingSize { w = model.landscapeCharSize.w, h = model.landscapeCharSize.h }

        row : Int
        row =
            floor ((absoluteMouseCoord.y - model.landscapeCharSize.h / 2.0) / triangleSize.h)

        col : Int
        col =
            if isEven row then
                floor ((absoluteMouseCoord.x - model.landscapeCharSize.w / 2.0 + triangleSize.w * toFloat (row // 2)) / triangleSize.w)
            else
                floor ((absoluteMouseCoord.x - model.landscapeCharSize.w / 2.0 + triangleSize.w * toFloat ((row - 1) // 2) + triangleSize.w / 2.0) / triangleSize.w)

        leftTriangle =
            getTriangle model.landscapeCharSize ( col - 1, row, CellRight )

        middleTriangle =
            getTriangle model.landscapeCharSize ( col, row, CellLeft )

        rightTriangle =
            getTriangle model.landscapeCharSize ( col, row, CellRight )
    in
        if isInTriangle leftTriangle absoluteMouseCoord then
            ( col - 1, row, CellRight )
        else if isInTriangle middleTriangle absoluteMouseCoord then
            ( col, row, CellLeft )
        else
            ( col, row, CellRight )


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
            { model' | hoveredCell = getHoveredCell model' }


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


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ charSizeResult CharSizeResult
        , Mouse.moves MouseMove
        , landscapeMousePosResult LandscapeMousePos
        ]
