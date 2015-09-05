module Data.IntSet.Data where

import qualified Data.IntSet as I


isetTo :: Int -> I.IntSet
isetTo n = I.fromList [1..n]

iset1 :: I.IntSet
iset1 = isetTo 10

iset2 :: I.IntSet
iset2 = isetTo 20

iset3 :: I.IntSet
iset3 = isetTo 30

iset4 :: I.IntSet
iset4 = isetTo 40

iset5 :: I.IntSet
iset5 = isetTo 50
