port module GoodKnight exposing (..)

import Dict
import Array exposing (Array)
import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Keyboard exposing (KeyCode)
import Mouse
import Time
import Random
import Random.Array
import Cards exposing (..)
import Board exposing (setCell, Board, CellCoordinates, CellPosition(..))
import Rules
import PixelMap
import Render
import LaunchWizard


main : Program Never
main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Player =
    { name : String
    , actionCards : List ActionCard
    }


initPlayer : String -> Player
initPlayer name =
    { name = name, actionCards = [ { colors = ( Cards.Red, Cards.Green ), action = Dragon } ] }


type alias Model =
    { board : Board
    , topLeft : ( Int, Int )
    , mousePressed : Bool
    , mousePressedInitialBoard : ( Int, Int )
    , mousePressedInitialPos : ( Int, Int )
    , mouseCurrentPos : ( Int, Int )
    , draggingBoard : Bool
    , landscapeMousePos : ( Int, Int )
    , landscapeFontName : String
    , landscapeFontSize : Int
    , landscapeCharSize : FloatSize
    , hoveredCellCoord : CellCoordinates
    , running : Bool
    , players : List Player
    , currentPlayer : Maybe String
    , currentRot : Int
    , currentDeck : List LandscapeCard
    , wizardModel : LaunchWizard.Model
    , possibleMoves : List Rules.Move
    }


type Msg
    = Move Int Int
    | MousePress Bool
    | MouseMove { x : Int, y : Int }
    | KeyDown KeyCode
    | CharSizeResult ( Float, Float )
    | LandscapeMousePos ( Int, Int )
    | RequestCharSize
    | BoundingClientRect ClientRect
    | LaunchGame (List String) LandscapeDeck
    | WizardMsg LaunchWizard.Msg
    | Tick Time.Time
    | NoOp


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
    { board =
        Board.init Board.CellLeft 0
        --            |> tmpInitBoard
    , topLeft = ( 0, 0 )
    , mousePressed = False
    , mousePressedInitialPos = ( 0, 0 )
    , mousePressedInitialBoard = ( 0, 0 )
    , mouseCurrentPos = ( 0, 0 )
    , draggingBoard = False
    , landscapeFontName = defaultLandscapeFontName
    , landscapeFontSize = defaultLandscapeFontSize
    , landscapeMousePos = ( 0, 0 )
    , landscapeCharSize = { w = 7.5, h = 14.0 }
    , hoveredCellCoord = ( 0, 0, CellLeft )
    , players = []
    , currentPlayer = Nothing
    , currentRot = 0
    , currentDeck = []
    , running = False
    , wizardModel = LaunchWizard.init
    , possibleMoves = []
    }
        ! [ requestCharSize ( defaultLandscapeFontName, defaultLandscapeFontSize )
          , Random.generate (LaunchGame [ "toto", "titi" ]) <| Random.Array.shuffle initialLandscapeDeck
          ]


landscapeStyle : Model -> Html.Attribute msg
landscapeStyle model =
    style
        [ ( "overflow", "hidden" )
        , ( "font-family", model.landscapeFontName )
        , ( "font-size", (toString model.landscapeFontSize) ++ "px" )
        , ( "flex", "1" )
        , ( "position", "relative" )
        , ( "background-color", "#FFFFFF" )
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


topBarStyle : Model -> Html.Attribute msg
topBarStyle model =
    style
        [ ( "background-color", "#E0E0E0" )
        , ( "font-family", model.landscapeFontName )
        , ( "font-size", (toString model.landscapeFontSize) ++ "px" )
        ]


leftItems : List (Html msg)
leftItems =
    Dict.empty
        |> PixelMap.pokeLeftCell ( 0, 1 ) backCard
        |> PixelMap.pokeLeftCell ( 1, 1 ) backCard
        |> PixelMap.pokeLeftCell ( 2, 1 ) backCard
        |> Render.renderMapToHtml ( 0, 0 )


huvStyle : Model -> Html.Attribute msg
huvStyle model =
    style
        [ ( "position", "absolute" )
        , ( "right", "10" )
        , ( "top", "10" )
        , ( "width", "200" )
        , ( "height", "400" )
        , ( "background-color", "rgba(200, 200, 200, 0.8)" )
        ]


viewBoard : Model -> Html Msg
viewBoard model =
    let
        mouseCurrentCharPos =
            mousePosToCharPos model.landscapeCharSize model.topLeft model.mouseCurrentPos

        currentCard =
            List.head model.currentDeck
                |> Maybe.withDefault backCard

        rotatedCurrentCard =
            rotateLandscapeCard model.currentRot currentCard
    in
        div
            [ style
                [ ( "overflow", "hidden" )
                , ( "display", "flex" )
                , ( "flex-direction", "column" )
                , ( "height", "100vh" )
                ]
            ]
            [ div [ topBarStyle model ]
                [ text <| "Current player: " ++ (Maybe.withDefault "<unamed player>" model.currentPlayer) ]
            , pre
                [ onMouseDown (MousePress True)
                , onMouseUp (MousePress False)
                , landscapeStyle model
                , id "landscape"
                ]
                (List.append
                    (Dict.empty
                        |> PixelMap.pixelizeBoard (Rules.movesToBoard model.possibleMoves currentCard)
                        |> PixelMap.grayIt
                        |> PixelMap.pixelizeBoard model.board
                        |> PixelMap.pixelizeCell model.hoveredCellCoord rotatedCurrentCard
                        |> Render.renderMapToHtml model.topLeft
                    )
                    [ div [ huvStyle model ]
                        [ text "Ok" ]
                    ]
                )
            ]


viewWizard : Model -> Html Msg
viewWizard model =
    LaunchWizard.view model.wizardModel |> App.map WizardMsg


view : Model -> Html Msg
view model =
    if model.running then
        viewBoard model
    else
        viewWizard model


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


getHoveredCellCoord : Model -> CellCoordinates
getHoveredCellCoord model =
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

        distance =
            toFloat ((initialX - x) ^ 2 + (initialY - y) ^ 2) |> sqrt

        deltaX =
            floor ((toFloat (x - initialX)) / model.landscapeCharSize.w)

        deltaY =
            floor ((toFloat (y - initialY)) / model.landscapeCharSize.h)
    in
        if model.draggingBoard then
            { model
                | topLeft =
                    ( fst model.mousePressedInitialBoard - deltaX
                    , snd model.mousePressedInitialBoard - deltaY
                    )
            }
        else
            { model
                | draggingBoard = distance > 3.0
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
            { model' | hoveredCellCoord = getHoveredCellCoord model' }


handleKeyDown : Int -> Model -> ( Model, Cmd Msg )
handleKeyDown keyCode model =
    case keyCode of
        37 ->
            { model | currentRot = (model.currentRot - 1) % 3 } ! []

        -- 38 ->
        --     shiftCardRefLeft
        39 ->
            { model | currentRot = (model.currentRot + 1) % 3 } ! []

        -- 40 ->
        --     shiftCardRefRight
        _ ->
            model ! []


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
            let
                currentCard =
                    List.head model.currentDeck
                        |> Maybe.withDefault backCard

                rotatedCurrentCard =
                    rotateLandscapeCard model.currentRot currentCard

                createCard =
                    (model.draggingBoard == False) && (pressed == False)

                commonModel =
                    { model
                        | mousePressed = pressed
                        , draggingBoard = False
                    }

                newModel =
                    if pressed then
                        { commonModel
                            | mousePressedInitialPos = commonModel.mouseCurrentPos
                            , mousePressedInitialBoard = commonModel.topLeft
                        }
                    else if createCard then
                        { commonModel
                            | board = Board.setLandscape commonModel.hoveredCellCoord rotatedCurrentCard commonModel.board
                            , currentDeck = Maybe.withDefault [] (List.tail commonModel.currentDeck)
                        }
                    else
                        commonModel
            in
                { newModel
                    | possibleMoves =
                        if createCard then
                            Rules.getPossibleMoves newModel.board <| Maybe.withDefault backCard <| List.head newModel.currentDeck
                        else
                            newModel.possibleMoves
                }
                    ! []

        KeyDown code ->
            handleKeyDown code model

        LaunchGame names deck ->
            launchGame names deck model

        WizardMsg msg ->
            handleWizardUpdate model msg

        BoundingClientRect clientRect ->
            handleBoundingClientRect clientRect model

        Tick _ ->
            { model | possibleMoves = Rules.rotateMoves model.possibleMoves } ! []


handleBoundingClientRect : ClientRect -> Model -> ( Model, Cmd Msg )
handleBoundingClientRect clientRect model =
    if clientRect.id == "landscape" then
        let
            triangleSize =
                triangleBoundingSize { w = model.landscapeCharSize.w, h = model.landscapeCharSize.h }

            l =
                round <| ((clientRect.rect.width - triangleSize.w) / 2.0) / model.landscapeCharSize.w

            t =
                round <| ((clientRect.rect.height - triangleSize.h) / 2.0) / model.landscapeCharSize.h
        in
            { model | topLeft = ( -l, -t ) } ! []
    else
        model ! []


handleWizardUpdate : Model -> LaunchWizard.Msg -> ( Model, Cmd Msg )
handleWizardUpdate model msg =
    let
        ( newWizardModel, wizardMsg ) =
            LaunchWizard.update msg model.wizardModel

        newModel =
            { model | wizardModel = newWizardModel }
    in
        case wizardMsg of
            Nothing ->
                newModel ! []

            Just (LaunchWizard.Launch names) ->
                newModel
                    ! [ Random.generate (LaunchGame names) <| Random.Array.shuffle initialLandscapeDeck ]


launchGame : List String -> LandscapeDeck -> Model -> ( Model, Cmd Msg )
launchGame names deck model =
    { model
        | running = True
        , players = List.map initPlayer names
        , currentDeck = Array.toList deck
        , currentPlayer = List.head names
        , possibleMoves = Rules.getPossibleMoves model.board <| Maybe.withDefault backCard <| Array.get 0 deck
    }
        ! [ requestBoundingClientRect "landscape" ]


spaceToInc : { x : Int, y : Int } -> Msg
spaceToInc { x, y } =
    Move x y


port requestCharSize : ( String, Int ) -> Cmd msg


port charSizeResult : (( Float, Float ) -> msg) -> Sub msg


port requestLandscapeMousePos : ( Int, Int ) -> Cmd msg


port landscapeMousePosResult : (( Int, Int ) -> msg) -> Sub msg


port requestBoundingClientRect : String -> Cmd msg


type alias ClientRect =
    { id : String
    , rect :
        { left : Float
        , top : Float
        , right : Float
        , bottom : Float
        , width : Float
        , height : Float
        }
    }


port boundingClientRectResult : (ClientRect -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ charSizeResult CharSizeResult
        , Mouse.moves MouseMove
        , Keyboard.downs KeyDown
        , landscapeMousePosResult LandscapeMousePos
        , boundingClientRectResult BoundingClientRect
        , Time.every Time.second Tick
        ]
