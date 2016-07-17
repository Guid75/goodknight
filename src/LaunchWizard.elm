module LaunchWizard exposing (view, Model, init, Msg, OutputMsg(..), update)

import Json.Decode
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String
import Maybe
import Result
import Array exposing (Array)
import Validate exposing (ifBlank, Validator)


type alias Model =
    { playerCount : Int
    , names : Array String
    }


defaultNames : Array String
defaultNames =
    Array.fromList [ "Alice", "Georges", "Daniel", "Marguerite" ]


init : Model
init =
    { playerCount = 2
    , names = defaultNames
    }


type Msg
    = SelectPlayerCount Int
    | ChangeName Int String
    | Start


type OutputMsg
    = Launch (List String)


parseIntWithDefault : String -> Int
parseIntWithDefault str =
    String.toInt str
        |> Result.toMaybe
        |> Maybe.withDefault 2


playerInput : Model -> Int -> Html Msg
playerInput model index =
    let
        name =
            Maybe.withDefault "Toto" <| Array.get index model.names
    in
        div []
            [ text <| "Player " ++ (toString <| index + 1)
            , input [ value name, onInput (ChangeName index) ] []
            , span
                [ style
                    [ ( "color", "red" )
                    ]
                ]
                [ ifBlank "Should not be empty" name |> List.head |> Maybe.withDefault "" |> text ]
            ]


nameValidator : Validator String String
nameValidator =
    ifBlank "Should not be empty"


isSubmitable : List String -> Bool
isSubmitable names =
    let
        validateName : String -> Bool
        validateName name =
            List.length (nameValidator name) == 0
    in
        List.all validateName names


view : Model -> Html Msg
view model =
    div []
        (List.concat
            [ [ text "How many players will participate?"
              , select
                    [ value <| toString model.playerCount
                    , on "change" (Json.Decode.object1 (SelectPlayerCount << parseIntWithDefault) (Json.Decode.at [ "target", "value" ] Json.Decode.string))
                    ]
                    [ option [ value "2" ] [ text "2 players" ]
                    , option [ value "3" ] [ text "3 players" ]
                    , option [ value "4" ] [ text "4 players" ]
                    ]
              ]
            , (List.map (playerInput model)
                [0..model.playerCount - 1]
              )
            , [ button
                    [ onClick Start
                    , model.names |> Array.toList |> isSubmitable |> not |> disabled
                    ]
                    [ text "Start" ]
              ]
            ]
        )


update : Msg -> Model -> ( Model, Maybe OutputMsg )
update msg model =
    case msg of
        SelectPlayerCount c ->
            ( { model | playerCount = c }, Nothing )

        ChangeName playerIndex newName ->
            ( { model | names = Array.set playerIndex newName model.names }, Nothing )

        Start ->
            ( model
            , Array.toList model.names
                |> List.take model.playerCount
                |> Launch
                |> Just
            )
