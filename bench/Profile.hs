module Main where

import Data.Set.Class as Sets

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
    [ bgroup "`Data.Set`" $ benchUnion set1 [set1,set2,set3,set4,set5]
    , bgroup "`Data.Map`" $ benchUnion map1 [map1,map2,map3,map4,map5]
    , bgroup "`Data.IntSet`" $ benchUnion iset1 [iset1,iset2,iset3,iset4,iset5]
    , bgroup "`Data.IntMap`" $ benchUnion imap1 [imap1,imap2,imap3,imap4,imap5]
    , bgroup "`Data.Set.Ordered.Many`" $ benchUnion omset1 [omset1,omset2,omset3,omset4,omset5]
    , bgroup "`Data.Set.Unordered.Many`" $ benchUnion umset1 [umset1,umset2,umset3,umset4,umset5]
    , bgroup "`Data.Set.Unordered.Unique`" $ benchUnion uuset1 [uuset1,uuset2,uuset3,uuset4,uuset5]
    ]
  , bgroup "Intersection"
    [ bgroup "`Data.Set`" $ benchIntersection set1 [set1,set2,set3,set4,set5]
    , bgroup "`Data.Map`" $ benchIntersection map1 [map1,map2,map3,map4,map5]
    , bgroup "`Data.IntSet`" $ benchIntersection iset1 [iset1,iset2,iset3,iset4,iset5]
    , bgroup "`Data.IntMap`" $ benchIntersection imap1 [imap1,imap2,imap3,imap4,imap5]
    , bgroup "`Data.Set.Ordered.Many`" $ benchIntersection omset1 [omset1,omset2,omset3,omset4,omset5]
    , bgroup "`Data.Set.Unordered.Many`" $ benchIntersection umset1 [umset1,umset2,umset3,umset4,umset5]
    , bgroup "`Data.Set.Unordered.Unique`" $ benchIntersection uuset1 [uuset1,uuset2,uuset3,uuset4,uuset5]
    ]
  , bgroup "Difference"
    [ bgroup "`Data.Set`" $ benchDifference set1 [set1,set2,set3,set4,set5]
    , bgroup "`Data.Map`" $ benchDifference map1 [map1,map2,map3,map4,map5]
    , bgroup "`Data.IntSet`" $ benchDifference iset1 [iset1,iset2,iset3,iset4,iset5]
    , bgroup "`Data.IntMap`" $ benchDifference imap1 [imap1,imap2,imap3,imap4,imap5]
    , bgroup "`Data.Set.Ordered.Many`" $ benchDifference omset1 [omset1,omset2,omset3,omset4,omset5]
    , bgroup "`Data.Set.Unordered.Many`" $ benchDifference umset1 [umset1,umset2,umset3,umset4,umset5]
    , bgroup "`Data.Set.Unordered.Unique`" $ benchDifference uuset1 [uuset1,uuset2,uuset3,uuset4,uuset5]
    ]
  ]


benchBin :: String -> (s -> s -> s) -> s -> [s] -> [Benchmark]
benchBin name bin s1 ss =
  [ bgroup (name ++ " xs")
    [ bench (show k) $ whnf (`bin` s1) s
          | (k,s) <- [1..] `zip` ss :: [(Int,s)]
          ]
  , bgroup ("xs " ++ name)
    [ bench (show k) $ whnf (s1 `bin`) s
          | (k,s) <- [1..] `zip` ss :: [(Int,s)]
          ]
  ]

benchUnion :: HasUnion s => s -> [s] -> [Benchmark]
benchUnion = benchBin "`union`" Sets.union

benchIntersection :: HasIntersection s => s -> [s] -> [Benchmark]
benchIntersection = benchBin "`intersection`" Sets.intersection

benchDifference :: HasDifference s => s -> [s] -> [Benchmark]
benchDifference = benchBin "`difference`" Sets.difference
