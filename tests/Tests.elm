module Tests where

import ElmTest exposing (..)

import CardsTestSuite
import BoardTestSuite
import RulesTestSuite

all : Test
all =
  suite "A Test Suite"
          [ CardsTestSuite.general
          , BoardTestSuite.general
          , RulesTestSuite.general
          ]
