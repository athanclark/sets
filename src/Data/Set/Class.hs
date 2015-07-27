{-# LANGUAGE
    NoImplicitPrelude
  , MultiParamTypeClasses
  , FlexibleInstances
  #-}

-- | Convenience operators overloaded for arbitrary use.
-- There are no laws associated with these classes, just duck-typed so
-- we don't have to use the qualified versions of each function.

module Data.Set.Class where

import Prelude (Ord)
import qualified Data.Set as Set
import qualified Data.Map.Lazy as MapLazy


class HasUnion s a where
  union :: s a -> s a -> s a

class HasDifference s a where
  difference :: s a -> s a -> s a

class HasIntersection s a where
  intersection :: s a -> s a -> s a

class HasSingleton s a where
  singleton :: a -> s a

class HasSingletonWith s k a where
  singletonWith :: k -> a -> s a

class HasEmpty s a where
  empty :: s a

class HasEmptyWith s k a where
  emptyWith :: k -> s a

-- Instances

-- Data.Set
instance Ord a => HasUnion Set.Set a where
  union = Set.union

instance Ord a => HasDifference Set.Set a where
  difference = Set.difference

instance Ord a => HasIntersection Set.Set a where
  intersection = Set.intersection

instance HasSingleton Set.Set a where
  singleton = Set.singleton

instance HasEmpty Set.Set a where
  empty = Set.empty

-- Data.Map.Lazy
instance Ord k => HasUnion (MapLazy.Map k) a where
  union = MapLazy.union

instance Ord k => HasDifference (MapLazy.Map k) a where
  difference = MapLazy.difference

instance Ord k => HasIntersection (MapLazy.Map k) a where
  intersection = MapLazy.intersection

instance HasSingletonWith (MapLazy.Map k) k a where
  singletonWith = MapLazy.singleton

instance HasEmpty (MapLazy.Map k) a where
  empty = MapLazy.empty
