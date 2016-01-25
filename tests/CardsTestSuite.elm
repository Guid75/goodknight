module CardsTestSuite (general) where

import ElmTest exposing (..)

import Cards exposing (..)

shift3 : Test
shift3 =
  suite "Testing shift3"
          [ test "shift3 should return (1, 2, 3) for (1, 2, 3) with a tick value of 0"
                   (assertEqual (Cards.shift3 0 (1, 2, 3)) (1, 2, 3))
          , test "shift3 should return (3, 1, 2) for (1, 2, 3) with a tick value of 1"
                   (assertEqual (Cards.shift3 1 (1, 2, 3)) (3, 1, 2))
          , test "shift3 should return (2, 3, 1) for (1, 2, 3) with a tick value of 2"
                   (assertEqual (Cards.shift3 2 (1, 2, 3)) (2, 3, 1))
          , test "shift3 should return (1, 2, 3) for (1, 2, 3) with a tick value of 3"
                   (assertEqual (Cards.shift3 3 (1, 2, 3)) (1, 2, 3))
          , test "shift3 should return (2, 3, 1) for (1, 2, 3) with a tick value of -1"
                   (assertEqual (Cards.shift3 -1 (1, 2, 3)) (2, 3, 1))
          , test "shift3 should return (3, 1, 2) for (1, 2, 3) with a tick value of -2"
                   (assertEqual (Cards.shift3 -2 (1, 2, 3)) (3, 1, 2))
          ]

-- Just a simple test because it really looks like the shift3 test
rotateLandscapeCard : Test
rotateLandscapeCard =
  suite "Testing rotateLandscapeCard"
          [
           test "rotateLandscapeCard return { corners = (Cross Yellow, Cross Red, Cross Green), edges = (Cross Red, Neutral, Cross Yellow), center = Neutral } for { corners = (Cross Green, Cross Yellow, Cross Red), edges = (Neutral, Cross Yellow, Cross Red), center = Neutral } with a tick value of 1"
            (assertEqual (Cards.rotateLandscapeCard 1 { corners = (Cross Green, Cross Yellow, Cross Red), edges = (Neutral, Cross Yellow, Cross Red), center = Neutral }) { corners = (Cross Red, Cross Green, Cross Yellow), edges = (Cross Red, Neutral, Cross Yellow), center = Neutral })
          ]

general : Test
general =
  suite "Testing Cards module"
          [ shift3
          , rotateLandscapeCard
          ]
