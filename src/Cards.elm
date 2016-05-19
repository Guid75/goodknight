module Cards exposing (..)

import Array
import Color


type Color
    = Red
    | Yellow
    | Blue
    | Green


type LandscapeItem
    = Neutral
    | Castle
    | Horse
    | Tournament
    | Cross Color
    | CardBack


type Challenge
    = TreasureChallenge
    | GiantChallenge
    | WitchChallenge
    | DragonChallenge
    | ThiefChallenge
    | PrincessChallenge


type alias LandscapeCorners =
    ( LandscapeItem, LandscapeItem, LandscapeItem )


type alias LandscapeEdges =
    ( LandscapeItem, LandscapeItem, LandscapeItem )


type alias LandscapeSide =
    ( LandscapeItem, LandscapeItem, LandscapeItem )


type alias LandscapeCenter =
    LandscapeItem


type alias LandscapeCard =
    { corners : LandscapeCorners
    , edges : LandscapeEdges
    , center : LandscapeCenter
    }


colorToElmColor : Color -> Color.Color
colorToElmColor c =
    case c of
        Red ->
            Color.red

        Yellow ->
            Color.yellow

        Blue ->
            Color.blue

        Green ->
            Color.green


allNeutral : ( LandscapeItem, LandscapeItem, LandscapeItem )
allNeutral =
    ( Neutral, Neutral, Neutral )


neutralCard : LandscapeCard
neutralCard =
    { corners = allNeutral
    , edges = allNeutral
    , center = Neutral
    }


backCard : LandscapeCard
backCard =
    { corners = allNeutral
    , edges = allNeutral
    , center = CardBack
    }


initialLandscapeDeck : Array.Array LandscapeCard
initialLandscapeDeck =
    Array.fromList
        [ { corners = ( Neutral, Tournament, Neutral )
          , edges = allNeutral
          , center = Castle
          }
        , { corners = allNeutral
          , edges = ( Neutral, Cross Yellow, Cross Red )
          , center = Neutral
          }
        , { corners = ( Tournament, Neutral, Neutral )
          , edges = allNeutral
          , center = Neutral
          }
        , { corners = allNeutral
          , edges = ( Neutral, Cross Red, Cross Green )
          , center = Neutral
          }
        , { corners = ( Neutral, Neutral, Tournament )
          , edges = allNeutral
          , center = Neutral
          }
        , { corners = ( Neutral, Tournament, Neutral )
          , edges = ( Neutral, Neutral, Cross Blue )
          , center = Neutral
          }
        , { corners = allNeutral
          , edges = ( Cross Red, Neutral, Neutral )
          , center = Horse
          }
        , { corners = allNeutral
          , edges = ( Cross Green, Neutral, Cross Blue )
          , center = Neutral
          }
        , { corners = allNeutral
          , edges = ( Cross Yellow, Neutral, Cross Blue )
          , center = Neutral
          }
        , { corners = ( Neutral, Neutral, Tournament )
          , edges = allNeutral
          , center = Horse
          }
        , { corners = allNeutral
          , edges = ( Neutral, Neutral, Cross Blue )
          , center = Horse
          }
        , { corners = ( Tournament, Neutral, Neutral )
          , edges = allNeutral
          , center = Neutral
          }
        , { corners = ( Tournament, Neutral, Neutral )
          , edges = ( Neutral, Cross Green, Neutral )
          , center = Neutral
          }
        , { corners = allNeutral
          , edges = ( Neutral, Cross Yellow, Neutral )
          , center = Neutral
          }
        , { corners = ( Neutral, Tournament, Neutral )
          , edges = ( Neutral, Neutral, Cross Red )
          , center = Neutral
          }
        , { corners = allNeutral
          , edges = ( Cross Yellow, Neutral, Neutral )
          , center = Neutral
          }
        , { corners = allNeutral
          , edges = ( Neutral, Cross Blue, Neutral )
          , center = Neutral
          }
        , { corners = allNeutral
          , edges = ( Neutral, Cross Green, Neutral )
          , center = Horse
          }
        , { corners = ( Neutral, Neutral, Tournament )
          , edges = ( Cross Green, Neutral, Neutral )
          , center = Neutral
          }
        , { corners = allNeutral
          , edges = ( Neutral, Cross Blue, Cross Red )
          , center = Neutral
          }
        , { corners = allNeutral
          , edges = ( Cross Yellow, Neutral, Neutral )
          , center = Horse
          }
        , { corners = ( Neutral, Tournament, Neutral )
          , edges = allNeutral
          , center = Horse
          }
        , { corners = ( Neutral, Tournament, Neutral )
          , edges = allNeutral
          , center = Horse
          }
        , { corners = allNeutral
          , edges = ( Neutral, Neutral, Cross Blue )
          , center = Neutral
          }
        , { corners = allNeutral
          , edges = ( Neutral, Cross Green, Cross Yellow )
          , center = Neutral
          }
        , { corners = ( Neutral, Neutral, Tournament )
          , edges = allNeutral
          , center = Horse
          }
        , { corners = ( Tournament, Neutral, Neutral )
          , edges = allNeutral
          , center = Neutral
          }
        , { corners = allNeutral
          , edges = ( Neutral, Neutral, Cross Red )
          , center = Neutral
          }
        , { corners = ( Neutral, Neutral, Tournament )
          , edges = ( Cross Yellow, Neutral, Neutral )
          , center = Neutral
          }
        , { corners = ( Neutral, Neutral, Tournament )
          , edges = ( Cross Red, Neutral, Neutral )
          , center = Neutral
          }
        , { corners = ( Tournament, Neutral, Neutral )
          , edges = allNeutral
          , center = Neutral
          }
        , { corners = ( Neutral, Tournament, Neutral )
          , edges = ( Neutral, Neutral, Cross Yellow )
          , center = Neutral
          }
        , { corners = allNeutral
          , edges = ( Neutral, Neutral, Cross Green )
          , center = Neutral
          }
        , { corners = ( Neutral, Tournament, Neutral )
          , edges = ( Neutral, Neutral, Cross Blue )
          , center = Neutral
          }
        , { corners = allNeutral
          , edges = ( Neutral, Cross Green, Neutral )
          , center = Neutral
          }
        , { corners = allNeutral
          , edges = ( Neutral, Cross Red, Neutral )
          , center = Neutral
          }
        ]


type alias ChallengeCard =
    { colors : ( Color, Color )
    , challenge : Challenge
    }



-- 3 same cards by challenge => 18 challenge cards


challengeCards : List ChallengeCard
challengeCards =
    [ { colors = ( Yellow, Blue )
      , challenge = GiantChallenge
      }
    , { colors = ( Green, Blue )
      , challenge = TreasureChallenge
      }
    , { colors = ( Blue, Red )
      , challenge = WitchChallenge
      }
    , { colors = ( Green, Red )
      , challenge = DragonChallenge
      }
    , { colors = ( Yellow, Green )
      , challenge = ThiefChallenge
      }
    , { colors = ( Yellow, Red )
      , challenge = PrincessChallenge
      }
    ]


shift3 : Int -> ( a, a, a ) -> ( a, a, a )
shift3 ticks trio =
    let
        shift ticks ( x, y, z ) =
            case ticks of
                0 ->
                    ( x, y, z )

                n ->
                    if n < 0 then
                        shift (n + 1) ( y, z, x )
                    else
                        shift (n - 1) ( z, x, y )
    in
        shift (ticks % 3) trio


rotateLandscapeCard : Int -> LandscapeCard -> LandscapeCard
rotateLandscapeCard ticks card =
    { card
        | corners = shift3 ticks card.corners
        , edges = shift3 ticks card.edges
    }


getLandscapeCardAndRotate : Int -> Int -> LandscapeCard
getLandscapeCardAndRotate index rot =
    Array.get index initialLandscapeDeck
        |> Maybe.map (rotateLandscapeCard rot)
        |> Maybe.withDefault neutralCard


getLandscapeSide : Int -> LandscapeCard -> LandscapeSide
getLandscapeSide sideIndex card =
    let
        index =
            sideIndex % 3

        ( corner1, corner2, corner3 ) =
            card.corners

        ( edge1, edge2, edge3 ) =
            card.edges
    in
        if index == 0 then
            ( corner1, edge1, corner2 )
        else if index == 1 then
            ( corner2, edge2, corner3 )
        else
            ( corner3, edge3, corner1 )


compatibleSides : LandscapeSide -> Maybe LandscapeSide -> Bool
compatibleSides ( side1corner1, side1edge, side1corner2 ) maybeSide2 =
    case maybeSide2 of
        Nothing ->
            True

        Just ( side2corner1, side2edge, side2corner2 ) ->
            (side1corner1 == side2corner2)
                && (side1edge == side2edge)
                && (side1corner2 == side2corner1)
