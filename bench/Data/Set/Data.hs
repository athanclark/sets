module Data.Set.Data where

import qualified Data.Set as Set



setTo :: Int -> Set.Set Int
setTo n = Set.fromList [1..n]

set1 :: Set.Set Int
set1 = setTo 10

set2 :: Set.Set Int
set2 = setTo 20

set3 :: Set.Set Int
set3 = setTo 30

set4 :: Set.Set Int
set4 = setTo 40

set5 :: Set.Set Int
set5 = setTo 50
