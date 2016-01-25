module Rules where

import Cards exposing (..)
import Board exposing (..)

isPossibleMove : CellCoordinates -> Int -> Int -> Board -> Bool
isPossibleMove (x, y, pos) landscapeIndex rot board =
  False


-- 1/ Récupération de la liste des cellules voisines
-- 2/ pour chaque voisin, il faut être compatible avec la carte qu'on veut poser
-- 3/
