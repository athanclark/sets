module Main where

import Data.Set.Class

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.IntSet as IntSet
import qualified Data.IntMap as IntMap
import qualified Data.List as List
import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.Functor.Contravariant as Pred
import qualified Data.Set.Ordered.Many as OM
import qualified Data.Set.Unordered.Many as UM
import qualified Data.Set.Unordered.Unique as UU
import qualified Data.Set.Ordered.Unique.Finite as OUF
import qualified Data.Set.Ordered.Unique.With as SetWith
import qualified Data.Set.Ordered.Many.With as SetsWith

import Data.Monoid
import Data.Commutative

import Criterion.Main
import Data.Set.Data
import Data.Map.Data
import Data.IntSet.Data
import Data.IntMap.Data
import Data.Set.Ordered.Many.Data
import Data.Set.Unordered.Many.Data
import Data.Set.Unordered.Unique.Data


main :: IO ()
main = defaultMain
  [ bgroup "Union"
    [ bgroup "`Data.Set`"
      [ bgroup "`union` xs"
        [ bench "1" $ whnf (`union` set1) set1
        , bench "2" $ whnf (`union` set1) set2
        , bench "3" $ whnf (`union` set1) set3
        , bench "4" $ whnf (`union` set1) set4
        , bench "5" $ whnf (`union` set1) set5
        ]
      , bgroup "xs `union`"
        [ bench "1" $ whnf (set1 `union`) set1
        , bench "2" $ whnf (set1 `union`) set2
        , bench "3" $ whnf (set1 `union`) set3
        , bench "4" $ whnf (set1 `union`) set4
        , bench "5" $ whnf (set1 `union`) set5
        ]
      ]
    , bgroup "`Data.Map`"
      [ bgroup "`union` xs"
        [ bench "1" $ whnf (`union` map1) map1
        , bench "2" $ whnf (`union` map1) map2
        , bench "3" $ whnf (`union` map1) map3
        , bench "4" $ whnf (`union` map1) map4
        , bench "5" $ whnf (`union` map1) map5
        ]
      , bgroup "xs `union`"
        [ bench "1" $ whnf (map1 `union`) map1
        , bench "2" $ whnf (map1 `union`) map2
        , bench "3" $ whnf (map1 `union`) map3
        , bench "4" $ whnf (map1 `union`) map4
        , bench "5" $ whnf (map1 `union`) map5
        ]
      ]
    , bgroup "`Data.IntSet`"
      [ bgroup "`union` xs"
        [ bench "1" $ whnf (`union` iset1) iset1
        , bench "2" $ whnf (`union` iset1) iset2
        , bench "3" $ whnf (`union` iset1) iset3
        , bench "4" $ whnf (`union` iset1) iset4
        , bench "5" $ whnf (`union` iset1) iset5
        ]
      , bgroup "xs `union`"
        [ bench "1" $ whnf (iset1 `union`) iset1
        , bench "2" $ whnf (iset1 `union`) iset2
        , bench "3" $ whnf (iset1 `union`) iset3
        , bench "4" $ whnf (iset1 `union`) iset4
        , bench "5" $ whnf (iset1 `union`) iset5
        ]
      ]
    , bgroup "`Data.IntMap`"
      [ bgroup "`union` xs"
        [ bench "1" $ whnf (`union` imap1) imap1
        , bench "2" $ whnf (`union` imap1) imap2
        , bench "3" $ whnf (`union` imap1) imap3
        , bench "4" $ whnf (`union` imap1) imap4
        , bench "5" $ whnf (`union` imap1) imap5
        ]
      , bgroup "xs `union`"
        [ bench "1" $ whnf (imap1 `union`) imap1
        , bench "2" $ whnf (imap1 `union`) imap2
        , bench "3" $ whnf (imap1 `union`) imap3
        , bench "4" $ whnf (imap1 `union`) imap4
        , bench "5" $ whnf (imap1 `union`) imap5
        ]
      ]
    , bgroup "`Data.Set.Ordered.Many`"
      [ bgroup "`union` xs"
        [ bench "1" $ whnf (`union` omset1) omset1
        , bench "2" $ whnf (`union` omset1) omset2
        , bench "3" $ whnf (`union` omset1) omset3
        , bench "4" $ whnf (`union` omset1) omset4
        , bench "5" $ whnf (`union` omset1) omset5
        ]
      , bgroup "xs `union`"
        [ bench "1" $ whnf (omset1 `union`) omset1
        , bench "2" $ whnf (omset1 `union`) omset2
        , bench "3" $ whnf (omset1 `union`) omset3
        , bench "4" $ whnf (omset1 `union`) omset4
        , bench "5" $ whnf (omset1 `union`) omset5
        ]
      ]
    , bgroup "`Data.Set.Unordered.Many`"
      [ bgroup "`union` xs"
        [ bench "1" $ whnf (`union` umset1) umset1
        , bench "2" $ whnf (`union` umset1) umset2
        , bench "3" $ whnf (`union` umset1) umset3
        , bench "4" $ whnf (`union` umset1) umset4
        , bench "5" $ whnf (`union` umset1) umset5
        ]
      , bgroup "xs `union`"
        [ bench "1" $ whnf (umset1 `union`) umset1
        , bench "2" $ whnf (umset1 `union`) umset2
        , bench "3" $ whnf (umset1 `union`) umset3
        , bench "4" $ whnf (umset1 `union`) umset4
        , bench "5" $ whnf (umset1 `union`) umset5
        ]
      ]
    , bgroup "`Data.Set.Unordered.Unique`"
      [ bgroup "`union` xs"
        [ bench "1" $ whnf (`union` uuset1) uuset1
        , bench "2" $ whnf (`union` uuset1) uuset2
        , bench "3" $ whnf (`union` uuset1) uuset3
        , bench "4" $ whnf (`union` uuset1) uuset4
        , bench "5" $ whnf (`union` uuset1) uuset5
        ]
      , bgroup "xs `union`"
        [ bench "1" $ whnf (uuset1 `union`) uuset1
        , bench "2" $ whnf (uuset1 `union`) uuset2
        , bench "3" $ whnf (uuset1 `union`) uuset3
        , bench "4" $ whnf (uuset1 `union`) uuset4
        , bench "5" $ whnf (uuset1 `union`) uuset5
        ]
      ]
    ]
  ]
