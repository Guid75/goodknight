module LaunchWizard exposing (view, Model, init, Msg, OutputMsg(..), update)

import Json.Decode
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Debug
import String
import Result
import Maybe


type alias Model =
    { playerCount : Int
    }


init : Model
init =
    { playerCount = 2
    }


type Msg
    = SelectPlayerCount Int
    | Start


type OutputMsg
    = NoOp
    | Launch


parseIntWithDefault : String -> Int
parseIntWithDefault str =
    String.toInt str
        |> Result.toMaybe
        |> Maybe.withDefault 2


playerInput : Model -> Int -> Html Msg
playerInput model index =
    div []
        [ text <| "Player " ++ (toString <| index + 1)
        , input [ value "toto" ] []
        ]


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
            , [ button [ onClick Start ] [ text "Start" ]
              ]
            ]
        )


update : Msg -> Model -> ( Model, OutputMsg )
update msg model =
    case msg of
        SelectPlayerCount c ->
            ( { model | playerCount = c }, NoOp )

        Start ->
            ( model, Launch )
