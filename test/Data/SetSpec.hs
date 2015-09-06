module Data.SetSpec (spec) where

import Data.Set.Class

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck
import Test.QuickCheck.Instances

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
import Data.Maybe
import Data.Commutative
import Control.Monad


spec :: [TestTree]
spec =
  [ testGroup "Union"
    [ testGroup "is associative"
      [ QC.testProperty "`Data.Set`"                  assUnionSet
      , QC.testProperty "`Data.Map`"                  assUnionMap
      , QC.testProperty "`Data.IntSet`"               assUnionIntSet
      , QC.testProperty "`Data.IntMap`"               assUnionIntMap
      , QC.testProperty "`Data.HashSet`"              assUnionHashSet
      , QC.testProperty "`Data.HashMap`"              assUnionHashMap
      -- , QC.testProperty "`Data.Set.Ordered.Many`"     assUnionOMSet -- FIXME: Dead x_x
      , QC.testProperty "`Data.Set.Unordered.Many`"   assUnionUMSet
      , QC.testProperty "`Data.Set.Unordered.Unique`" assUnionUUSet
      ]
    , testGroup "is commutative"
      [ QC.testProperty "`Data.Set`"                  comUnionSet
      , QC.testProperty "`Data.IntSet`"               comUnionIntSet
      , QC.testProperty "`Data.HashSet`"              comUnionHashSet
      , QC.testProperty "`Data.Set.Ordered.Many`"     comUnionOMSet
      , QC.testProperty "`Data.Set.Unordered.Many`"   comUnionUMSet
      ]
    ]
  , testGroup "Intersection"
    [ testGroup "is commutative"
      [ QC.testProperty "`Data.Set`"                  comIntersectionSet
      , QC.testProperty "`Data.IntSet`"               comIntersectionIntSet
      , QC.testProperty "`Data.HashSet`"              comIntersectionHashSet
      -- , QC.testProperty "`Data.Set.Ordered.Many`"     comIntersectionOMSet
      ]
    ]
  , testGroup "Symmetric Difference"
    [ testGroup "is associative"
      [ QC.testProperty "`Data.Set`"                  assXUnionSet
      , QC.testProperty "`Data.IntSet`"               assXUnionIntSet
      , QC.testProperty "`Data.HashSet`"              assXUnionHashSet
      ]
    , testGroup "is commutative"
      [ QC.testProperty "`Data.Set`"                  comXUnionSet
      , QC.testProperty "`Data.Map`"                  comXUnionMap
      , QC.testProperty "`Data.IntSet`"               comXUnionIntSet
      , QC.testProperty "`Data.IntMap`"               comXUnionIntMap
      , QC.testProperty "`Data.HashSet`"              comXUnionHashSet
      , QC.testProperty "`Data.HashMap`"              comXUnionHashMap
      -- , QC.testProperty "`Data.Set.Ordered.Many`"     comXUnionOMSet
      , QC.testProperty "`Data.Set.Unordered.Many`"   comXUnionUMSet
      ]
    ]
  , testGroup "Uniqueness"
    [ testGroup "Union"
      [ QC.testProperty "`Data.Set.Unordered.Unique`"   uniqueUnionUUSet
      ]
    , testGroup "Intersection"
      [ QC.testProperty "`Data.Set.Unordered.Unique`"   uniqueIntersectionUUSet
      ]
    , testGroup "Difference"
      [ QC.testProperty "`Data.Set.Unordered.Unique`"   uniqueDifferenceUUSet
      ]
    ]
  ]
  where
    assUnionSet :: Union (Set.Set Int) -> Union (Set.Set Int) -> Union (Set.Set Int) -> Bool
    assUnionSet = associates
    assUnionMap :: Union (Map.Map Int Int) -> Union (Map.Map Int Int) -> Union (Map.Map Int Int) -> Bool
    assUnionMap = associates
    assUnionIntSet :: Union IntSet.IntSet -> Union IntSet.IntSet -> Union IntSet.IntSet -> Bool
    assUnionIntSet = associates
    assUnionIntMap :: Union (IntMap.IntMap Int) -> Union (IntMap.IntMap Int) -> Union (IntMap.IntMap Int) -> Bool
    assUnionIntMap = associates
    assUnionHashSet :: Union (HashSet.HashSet Int) -> Union (HashSet.HashSet Int) -> Union (HashSet.HashSet Int) -> Bool
    assUnionHashSet = associates
    assUnionHashMap :: Union (HashMap.HashMap Int Int) -> Union (HashMap.HashMap Int Int) -> Union (HashMap.HashMap Int Int) -> Bool
    assUnionHashMap = associates
    assUnionOMSet :: Union (OM.OMSet Int) -> Union (OM.OMSet Int) -> Union (OM.OMSet Int) -> Bool
    assUnionOMSet = associates
    assUnionUMSet :: Union (UM.UMSet Int) -> Union (UM.UMSet Int) -> Union (UM.UMSet Int) -> Bool
    assUnionUMSet = associates
    assUnionUUSet :: Union (UU.UUSet Int) -> Union (UU.UUSet Int) -> Union (UU.UUSet Int) -> Bool
    assUnionUUSet = associates

    comUnionSet :: Union (Set.Set Int) -> Union (Set.Set Int) -> Bool
    comUnionSet = commutes
    comUnionIntSet :: Union IntSet.IntSet -> Union IntSet.IntSet -> Bool
    comUnionIntSet = commutes
    comUnionHashSet :: Union (HashSet.HashSet Int) -> Union (HashSet.HashSet Int) -> Bool
    comUnionHashSet = commutes
    comUnionOMSet :: Union (OM.OMSet Int) -> Union (OM.OMSet Int) -> Bool
    comUnionOMSet = commutes
    comUnionUMSet :: Union (UM.UMSet Int) -> Union (UM.UMSet Int) -> Bool
    comUnionUMSet = commutes

    comIntersectionSet :: Intersection (Set.Set Int) -> Intersection (Set.Set Int) -> Bool
    comIntersectionSet = commutes
    comIntersectionIntSet :: Intersection IntSet.IntSet -> Intersection IntSet.IntSet -> Bool
    comIntersectionIntSet = commutes
    comIntersectionHashSet :: Intersection (HashSet.HashSet Int) -> Intersection (HashSet.HashSet Int) -> Bool
    comIntersectionHashSet = commutes
    comIntersectionOMSet :: Intersection (OM.OMSet Int) -> Intersection (OM.OMSet Int) -> Bool
    comIntersectionOMSet = commutes
    comIntersectionUMSet :: Intersection (UM.UMSet Int) -> Intersection (UM.UMSet Int) -> Bool
    comIntersectionUMSet = commutes
    comIntersectionUUSet :: Intersection (UU.UUSet Int) -> Intersection (UU.UUSet Int) -> Bool
    comIntersectionUUSet = commutes

    assXUnionSet :: XUnion (Set.Set Int) -> XUnion (Set.Set Int) -> XUnion (Set.Set Int) -> Bool
    assXUnionSet = associates
    assXUnionIntSet :: XUnion IntSet.IntSet -> XUnion IntSet.IntSet -> XUnion IntSet.IntSet -> Bool
    assXUnionIntSet = associates
    assXUnionHashSet :: XUnion (HashSet.HashSet Int) -> XUnion (HashSet.HashSet Int) -> XUnion (HashSet.HashSet Int) -> Bool
    assXUnionHashSet = associates

    comXUnionSet :: XUnion (Set.Set Int) -> XUnion (Set.Set Int) -> Bool
    comXUnionSet = commutes
    comXUnionMap :: XUnion (Map.Map Int Int) -> XUnion (Map.Map Int Int) -> Bool
    comXUnionMap = commutes
    comXUnionIntSet :: XUnion IntSet.IntSet -> XUnion IntSet.IntSet -> Bool
    comXUnionIntSet = commutes
    comXUnionIntMap :: XUnion (IntMap.IntMap Int) -> XUnion (IntMap.IntMap Int) -> Bool
    comXUnionIntMap = commutes
    comXUnionHashSet :: XUnion (HashSet.HashSet Int) -> XUnion (HashSet.HashSet Int) -> Bool
    comXUnionHashSet = commutes
    comXUnionHashMap :: XUnion (HashMap.HashMap Int Int) -> XUnion (HashMap.HashMap Int Int) -> Bool
    comXUnionHashMap = commutes
    comXUnionOMSet :: XUnion (OM.OMSet Int) -> XUnion (OM.OMSet Int) -> Bool
    comXUnionOMSet = commutes
    comXUnionUMSet :: XUnion (UM.UMSet Int) -> XUnion (UM.UMSet Int) -> Bool
    comXUnionUMSet = commutes

    uniqueUnionUUSet :: UU.UUSet Int -> UU.UUSet Int -> Bool
    uniqueUnionUUSet x y = noDuplicates $ UU.unUUSet $ x `union` y
    uniqueIntersectionUUSet :: UU.UUSet Int -> UU.UUSet Int -> Bool
    uniqueIntersectionUUSet x y = noDuplicates $ UU.unUUSet $ x `intersection` y
    uniqueDifferenceUUSet :: UU.UUSet Int -> UU.UUSet Int -> Bool
    uniqueDifferenceUUSet x y = noDuplicates $ UU.unUUSet $ x `difference` y

    noDuplicates :: Ord a => [a] -> Bool
    noDuplicates xs = length xs == Set.size (Set.fromList xs)

    orderedUnionOMSet :: OM.OMSet Int -> OM.OMSet Int -> Bool
    orderedUnionOMSet x y = ascending $ OM.unOMSet $ x `union` y

    ascending :: ( Foldable f
                 , Ord a
                 , Bounded a
                 ) => f a -> Bool
    ascending = isJust . foldr (\a b -> isLess a =<< b) (Just minBound)
      where
        isLess x y = x <$ guard (x <= y)

associates :: (Eq a, Monoid a) => a -> a -> a -> Bool
associates x y z = x <> (y <> z) == (x <> y) <> z

commutes :: (Eq a, Commutative a) => a -> a -> Bool
commutes x y = x <~> y == y <~> x


instance Arbitrary a => Arbitrary (Union a) where
  arbitrary = Union <$> arbitrary

instance Arbitrary a => Arbitrary (Intersection a) where
  arbitrary = Intersection <$> arbitrary

instance Arbitrary a => Arbitrary (XUnion a) where
  arbitrary = XUnion <$> arbitrary

---------

newtype Under20 a = Under20 {unUnder20 :: [a]}
  deriving (Show, Eq)

instance Arbitrary a => Arbitrary (Under20 a) where
  arbitrary = Under20 <$> arbitrary `suchThat` (\x -> length x <= 20)
