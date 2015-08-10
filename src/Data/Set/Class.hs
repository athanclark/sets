{-# LANGUAGE
    NoImplicitPrelude
  , MultiParamTypeClasses
  , FlexibleInstances
  #-}

-- | Convenience operators overloaded for arbitrary use.
-- There are no laws associated with these classes, just duck-typed so
-- we don't have to use the qualified versions of each function.

module Data.Set.Class where

import Prelude (Eq (..), Ord, Int, Bool (..), (&&), (||), ($), (.), not, const)
import Data.Foldable as Fold
import Data.Monoid as Monoid

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.IntSet as IntSet
import qualified Data.IntMap as IntMap
import qualified Data.List as List
import Data.Hashable (Hashable)
import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.SetWith as SetWith
import qualified Data.Functor.Contravariant as Pred
import qualified Data.Set.Ordered.Many as OM
import Data.Discrimination as Disc
import qualified Data.Set.Unordered.Many as UM
import qualified Data.Set.Unordered.Unique as UU
import qualified Data.Set.Ordered.Unique.Finite as OUF


class HasUnion s where
  union :: s -> s -> s

unions :: ( Fold.Foldable f
          , HasUnion s
          , HasEmpty s
          ) => f s -> s
unions = foldr Data.Set.Class.union empty

class HasDifference s where
  difference :: s -> s -> s

class HasIntersection s where
  intersection :: s -> s -> s

intersections :: ( Fold.Foldable f
                 , HasIntersection s
                 , HasTotal s
                 ) => f s -> s
intersections = foldr Data.Set.Class.intersection total

class HasComplement s where
  complement :: s -> s

class HasSingleton s a where
  singleton :: a -> s

class HasSingletonWith s k a where
  singletonWith :: k -> a -> s

class HasEmpty s where
  empty :: s

class HasEmptyWith s k where
  emptyWith :: k -> s

class HasTotal s where
  total :: s

class HasTotalWith s k where
  totalWith :: k -> s

class HasSize s where
  size :: s -> Int

class CanBeSubset s where
  isSubsetOf :: s -> s -> Bool

class CanBeProperSubset s where
  isProperSubsetOf :: s -> s -> Bool


-- Instances

-- Data.Set
instance Ord a => HasUnion (Set.Set a) where
  union = Set.union

instance Ord a => HasDifference (Set.Set a) where
  difference = Set.difference

instance Ord a => HasIntersection (Set.Set a) where
  intersection = Set.intersection

instance HasSingleton (Set.Set a) a where
  singleton = Set.singleton

instance HasEmpty (Set.Set a) where
  empty = Set.empty

instance HasSize (Set.Set a) where
  size = Set.size

instance Ord a => CanBeSubset (Set.Set a) where
  isSubsetOf = Set.isSubsetOf

instance Ord a => CanBeProperSubset (Set.Set a) where
  isProperSubsetOf = Set.isProperSubsetOf


-- Data.Map
instance Ord k => HasUnion (Map.Map k a) where
  union = Map.union

instance Ord k => HasDifference (Map.Map k a) where
  difference = Map.difference

instance Ord k => HasIntersection (Map.Map k a) where
  intersection = Map.intersection

instance HasSingletonWith (Map.Map k a) k a where
  singletonWith = Map.singleton

instance HasEmpty (Map.Map k a) where
  empty = Map.empty

instance HasSize (Map.Map k a) where
  size = Map.size

instance (Eq k, Ord k, Eq a) => CanBeSubset (Map.Map k a) where
  isSubsetOf = Map.isSubmapOf

instance (Eq k, Ord k, Eq a) => CanBeProperSubset (Map.Map k a) where
  isProperSubsetOf = Map.isProperSubmapOf


-- Data.List
instance HasSingleton [a] a where
  singleton = (:[])

instance HasEmpty [a] where
  empty = []

instance HasSize [a] where
  size = List.length

-- Data.Sequence
instance HasSingleton (Seq.Seq a) a where
  singleton = Seq.singleton

instance HasEmpty (Seq.Seq a) where
  empty = Seq.empty

instance HasSize (Seq.Seq a) where
  size = Seq.length

-- Data.IntSet
instance HasUnion IntSet.IntSet where
  union = IntSet.union

instance HasDifference IntSet.IntSet where
  difference = IntSet.difference

instance HasIntersection IntSet.IntSet where
  intersection = IntSet.intersection

instance HasSingleton IntSet.IntSet IntSet.Key where
  singleton = IntSet.singleton

instance HasEmpty IntSet.IntSet where
  empty = IntSet.empty

instance HasSize IntSet.IntSet where
  size = IntSet.size

instance CanBeSubset IntSet.IntSet where
  isSubsetOf = IntSet.isSubsetOf

instance CanBeProperSubset IntSet.IntSet where
  isProperSubsetOf = IntSet.isProperSubsetOf


-- Data.IntMap
instance HasUnion (IntMap.IntMap a) where
  union = IntMap.union

instance HasDifference (IntMap.IntMap a) where
  difference = IntMap.difference

instance HasIntersection (IntMap.IntMap a) where
  intersection = IntMap.intersection

instance HasSingletonWith (IntMap.IntMap a) IntMap.Key a where
  singletonWith = IntMap.singleton

instance HasEmpty (IntMap.IntMap a) where
  empty = IntMap.empty

instance HasSize (IntMap.IntMap a) where
  size = IntMap.size

instance Eq a => CanBeSubset (IntMap.IntMap a) where
  isSubsetOf = IntMap.isSubmapOf

instance Eq a => CanBeProperSubset (IntMap.IntMap a) where
  isProperSubsetOf = IntMap.isProperSubmapOf


-- Data.HashSet
instance (Hashable a, Eq a) => HasUnion (HashSet.HashSet a) where
  union = HashSet.union

instance (Hashable a, Eq a) => HasDifference (HashSet.HashSet a) where
  difference = HashSet.difference

instance (Hashable a, Eq a) => HasIntersection (HashSet.HashSet a) where
  intersection = HashSet.intersection

instance Hashable a => HasSingleton (HashSet.HashSet a) a where
  singleton = HashSet.singleton

instance HasEmpty (HashSet.HashSet a) where
  empty = HashSet.empty

instance HasSize (HashSet.HashSet a) where
  size = HashSet.size


-- Data.HashMap
instance (Hashable k, Eq k) => HasUnion (HashMap.HashMap k a) where
  union = HashMap.union

instance (Hashable k, Eq k) => HasDifference (HashMap.HashMap k a) where
  difference = HashMap.difference

instance (Hashable k, Eq k) => HasIntersection (HashMap.HashMap k a) where
  intersection = HashMap.intersection

instance Hashable k => HasSingletonWith (HashMap.HashMap k a) k a where
  singletonWith = HashMap.singleton

instance HasEmpty (HashMap.HashMap k a) where
  empty = HashMap.empty

instance HasSize (HashMap.HashMap k a) where
  size = HashMap.size

-- Data.SetWith
instance Ord k => HasUnion (SetWith.SetWith k a) where
  union = SetWith.union

instance Ord k => HasDifference (SetWith.SetWith k a) where
  difference = SetWith.difference

instance Ord k => HasIntersection (SetWith.SetWith k a) where
  intersection = SetWith.intersection

instance Ord k => HasSingletonWith (SetWith.SetWith k a) (a -> k) a where
  singletonWith = SetWith.singleton

instance HasEmptyWith (SetWith.SetWith k a) (a -> k) where
  emptyWith = SetWith.empty

instance HasSize (SetWith.SetWith k a) where
  size = SetWith.size

instance (Ord k, Eq a) => CanBeSubset (SetWith.SetWith k a) where
  isSubsetOf = SetWith.isSubsetOf

instance (Ord k, Eq a) => CanBeProperSubset (SetWith.SetWith k a) where
  isProperSubsetOf = SetWith.isProperSubsetOf

-- Data.Functor.Contravariant.Predicate
instance HasUnion (Pred.Predicate a) where
  union (Pred.Predicate f) (Pred.Predicate g) = Pred.Predicate $ \x -> f x || g x

instance HasDifference (Pred.Predicate a) where
  difference (Pred.Predicate f) (Pred.Predicate g) = Pred.Predicate $ \x -> f x && not (g x)

instance HasIntersection (Pred.Predicate a) where
  intersection (Pred.Predicate f) (Pred.Predicate g) = Pred.Predicate $ \x -> f x && g x

instance HasComplement (Pred.Predicate a) where
  complement (Pred.Predicate f) = Pred.Predicate $ not . f

instance Eq a => HasSingleton (Pred.Predicate a) a where
  singleton a = Pred.Predicate $ \x -> a == x

instance HasEmpty (Pred.Predicate a) where
  empty = Pred.Predicate $ const False

instance HasTotal (Pred.Predicate a) where
  total = Pred.Predicate $ const True


-- Data.Set.Ordered.Many
instance Disc.Sorting a => HasUnion (OM.OMSet a) where
  union = OM.union

instance Eq a => HasDifference (OM.OMSet a) where
  difference = OM.difference

instance Ord a => HasIntersection (OM.OMSet a) where
  intersection = OM.intersection

instance HasSingleton (OM.OMSet a) a where
  singleton = OM.singleton

instance HasEmpty (OM.OMSet a) where
  empty = OM.empty

instance HasSize (OM.OMSet a) where
  size = OM.size

instance Eq a => CanBeSubset (OM.OMSet a) where
  isSubsetOf = OM.isSubsetOf

instance Eq a => CanBeProperSubset (OM.OMSet a) where
  isProperSubsetOf = OM.isProperSubsetOf


-- Data.Set.Unordered.Many
instance Eq a => HasUnion (UM.UMSet a) where
  union = UM.union

instance Eq a => HasDifference (UM.UMSet a) where
  difference = UM.difference

instance Eq a => HasIntersection (UM.UMSet a) where
  intersection = UM.intersection

instance HasSingleton (UM.UMSet a) a where
  singleton = UM.singleton

instance HasEmpty (UM.UMSet a) where
  empty = UM.empty

instance HasSize (UM.UMSet a) where
  size = UM.size

instance Eq a => CanBeSubset (UM.UMSet a) where
  isSubsetOf = UM.isSubsetOf

instance Eq a => CanBeProperSubset (UM.UMSet a) where
  isProperSubsetOf = UM.isProperSubsetOf


-- Data.Set.Unordered.Unique
instance Eq a => HasUnion (UU.UUSet a) where
  union = UU.union

instance Eq a => HasDifference (UU.UUSet a) where
  difference = UU.difference

instance Eq a => HasIntersection (UU.UUSet a) where
  intersection = UU.intersection

instance HasSingleton (UU.UUSet a) a where
  singleton = UU.singleton

instance HasEmpty (UU.UUSet a) where
  empty = UU.empty

instance HasSize (UU.UUSet a) where
  size = UU.size

instance Eq a => CanBeSubset (UU.UUSet a) where
  isSubsetOf = UU.isSubsetOf

instance Eq a => CanBeProperSubset (UU.UUSet a) where
  isProperSubsetOf = UU.isProperSubsetOf


-- Data.Set.Ordered.Unique.Finite
instance Ord a => HasUnion (OUF.FiniteSet a) where
  union = OUF.union

instance Ord a => HasDifference (OUF.FiniteSet a) where
  difference = OUF.difference

instance Ord a => HasIntersection (OUF.FiniteSet a) where
  intersection = OUF.intersection

instance HasSingletonWith (OUF.FiniteSet a) (Set.Set a) a where
  singletonWith = OUF.singleton

instance HasEmptyWith (OUF.FiniteSet a) (Set.Set a) where
  emptyWith = OUF.empty

instance HasTotalWith (OUF.FiniteSet a) (OUF.FiniteSet a) where
  totalWith (OUF.FiniteSet (t,_)) = OUF.FiniteSet (t,t)

instance HasSize (OUF.FiniteSet a) where
  size = OUF.size

instance Ord a => CanBeSubset (OUF.FiniteSet a) where
  isSubsetOf = OUF.isSubsetOf

instance Ord a => CanBeProperSubset (OUF.FiniteSet a) where
  isProperSubsetOf = OUF.isProperSubsetOf
