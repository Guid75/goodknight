module Render (..) where

import Board
import Dict

type alias RenderMap =
  Dict.Dict Int (Dict.Dict Int Char)

render : Board.Board -> RenderMap
render board =
  let
    map = Dict.empty
  in
    map
