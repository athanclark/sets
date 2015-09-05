module Data.Map.Data where

import qualified Data.Map as Map


mapTo :: Int -> Map.Map Int Int
mapTo n = Map.fromList $ [0..n] `zip` [0..n]

map1 :: Map.Map Int Int
map1 = mapTo 10

map2 :: Map.Map Int Int
map2 = mapTo 20

map3 :: Map.Map Int Int
map3 = mapTo 30

map4 :: Map.Map Int Int
map4 = mapTo 40

map5 :: Map.Map Int Int
map5 = mapTo 50
