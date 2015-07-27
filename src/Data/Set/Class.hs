{-# LANGUAGE
    NoImplicitPrelude
  , MultiParamTypeClasses
  , FlexibleInstances
  #-}

-- | Convenience operators overloaded for arbitrary use.
-- There are no laws associated with these classes, just duck-typed so
-- we don't have to use the qualified versions of each function.

module Data.Set.Class where

import Prelude (Eq, Ord)
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.IntSet as IntSet
import qualified Data.IntMap as IntMap
import Data.Hashable (Hashable)
import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Lazy as HashMap


class HasUnion s where
  union :: s -> s -> s

class HasDifference s where
  difference :: s -> s -> s

class HasIntersection s where
  intersection :: s -> s -> s

class HasSingleton s a where
  singleton :: a -> s

class HasSingletonWith s k a where
  singletonWith :: k -> a -> s

class HasEmpty s where
  empty :: s

class HasEmptyWith s k a where
  emptyWith :: k -> s a

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

-- Data.Sequence
instance HasSingleton (Seq.Seq a) a where
  singleton = Seq.singleton

instance HasEmpty (Seq.Seq a) where
  empty = Seq.empty

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
