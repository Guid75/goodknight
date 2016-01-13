module Cards where

type Color = Red
           | Yellow
           | Blue
           | Green

type LandscapeItem = Neutral
                   | Castle
                   | Horse
                   | Tournament
                   | Cross Color

type Challenge = TreasureChallenge
               | GiantChallenge
               | WitchChallenge
               | DragonChallenge
               | ThiefChallenge
               | PrincessChallenge

type alias LandscapeCorners =
  ( LandscapeItem, LandscapeItem, LandscapeItem)

type alias LandscapeEdges =
  ( LandscapeItem, LandscapeItem, LandscapeItem)

type alias LandscapeCenter = LandscapeItem

type alias LandscapeCard =
  { corners: LandscapeCorners
    -- the fist element of the edges tuple immediately geometrically follows the first element of the corners tuple
  , edges: LandscapeEdges
  , center: LandscapeCenter
  }

allNeutral : (LandscapeItem, LandscapeItem, LandscapeItem)
allNeutral = (Neutral, Neutral, Neutral)

initialDeck : List LandscapeCard
initialDeck =
  [ { corners = allNeutral
    , edges = (Neutral, Cross Yellow, Cross Red)
    , center = Neutral
    }
  , { corners = (Tournament, Neutral, Neutral)
    , edges = allNeutral
    , center = Neutral
    }
  , { corners = allNeutral
    , edges = (Neutral, Cross Red, Cross Green)
    , center = Neutral
    }
  , { corners = (Neutral, Neutral, Tournament)
    , edges = allNeutral
    , center = Neutral
    }
  , { corners = (Neutral, Tournament, Neutral)
    , edges = (Neutral, Neutral, Cross Blue)
    , center = Neutral
    }
  , { corners = allNeutral
    , edges = (Cross Red, Neutral, Neutral)
    , center = Horse
    }
  , { corners = allNeutral
    , edges = (Cross Green, Neutral, Cross Blue)
    , center = Neutral
    }
  , { corners = allNeutral
    , edges = (Cross Yellow, Neutral, Cross Blue)
    , center = Neutral
    }
  , { corners = (Neutral, Neutral, Tournament)
    , edges = allNeutral
    , center = Horse
    }
  , { corners = allNeutral
    , edges = (Neutral, Neutral, Cross Blue)
    , center = Horse
    }
  , { corners = (Tournament, Neutral, Neutral)
    , edges = allNeutral
    , center = Neutral
    }
  , { corners = (Tournament, Neutral, Neutral)
    , edges = (Neutral, Cross Green, Neutral)
    , center = Neutral
    }
  , { corners = allNeutral
    , edges = (Neutral, Cross Yellow, Neutral)
    , center = Neutral
    }
  , { corners = (Neutral, Tournament, Neutral)
    , edges = allNeutral
    , center = Castle
    }
  , { corners = (Neutral, Tournament, Neutral)
    , edges = (Neutral, Neutral, Cross Red)
    , center = Neutral
    }
  , { corners = allNeutral
    , edges = (Cross Yellow, Neutral, Neutral)
    , center = Neutral
    }
  , { corners = allNeutral
    , edges = (Neutral, Cross Blue, Neutral)
    , center = Neutral
    }
  , { corners = allNeutral
    , edges = (Neutral, Cross Green, Neutral)
    , center = Horse
    }
  , { corners = (Neutral, Neutral, Tournament)
    , edges = (Cross Green, Neutral, Neutral)
    , center = Neutral
    }
  , { corners = allNeutral
    , edges = (Neutral, Cross Blue, Cross Red)
    , center = Neutral
    }
  , { corners = allNeutral
    , edges = (Cross Yellow, Neutral, Neutral)
    , center = Horse
    }
  , { corners = (Neutral, Tournament, Neutral)
    , edges = allNeutral
    , center = Horse
    }
  , { corners = (Neutral, Tournament, Neutral)
    , edges = allNeutral
    , center = Horse
    }
  , { corners = allNeutral
    , edges = (Neutral, Neutral, Cross Blue)
    , center = Neutral
    }
  , { corners = allNeutral
    , edges = (Neutral, Cross Green, Cross Yellow)
    , center = Neutral
    }
  , { corners = (Neutral, Neutral, Tournament)
    , edges = allNeutral
    , center = Horse
    }
  , { corners = (Tournament, Neutral, Neutral)
    , edges = allNeutral
    , center = Neutral
    }
  , { corners = allNeutral
    , edges = (Neutral, Neutral, Cross Red)
    , center = Neutral
    }
  , { corners = (Neutral, Neutral, Tournament)
    , edges = (Cross Yellow, Neutral, Neutral)
    , center = Neutral
    }
  , { corners = (Neutral, Neutral, Tournament)
    , edges = (Cross Red, Neutral, Neutral)
    , center = Neutral
    }
  , { corners = (Tournament, Neutral, Neutral)
    , edges = allNeutral
    , center = Neutral
    }
  , { corners = (Neutral, Tournament, Neutral)
    , edges = (Neutral, Neutral, Cross Yellow)
    , center = Neutral
    }
  , { corners = allNeutral
    , edges = (Neutral, Neutral, Cross Green)
    , center = Neutral
    }
  , { corners = (Neutral, Tournament, Neutral)
    , edges = (Neutral, Neutral, Cross Blue)
    , center = Neutral
    }
  , { corners = allNeutral
    , edges = (Neutral, Cross Green, Neutral)
    , center = Neutral
    }
  , { corners = allNeutral
    , edges = (Neutral, Cross Red, Neutral)
    , center = Neutral
    }
  ]

type alias ChallengeCard =
  { colors: (Color, Color)
  , challenge: Challenge
  }

-- 3 same cards by challenge => 18 challenge cards
challengeCards : List ChallengeCard
challengeCards =
  [ { colors = (Yellow, Blue)
    , challenge = GiantChallenge
    }
  , { colors = (Green, Blue)
    , challenge = TreasureChallenge
    }
  , { colors = (Blue, Red)
    , challenge = WitchChallenge
    }
  , { colors = (Green, Red)
    , challenge = DragonChallenge
    }
  , { colors = (Yellow, Green)
    , challenge = ThiefChallenge
    }
  , { colors = (Yellow, Red)
    , challenge = PrincessChallenge
    }
  ]
