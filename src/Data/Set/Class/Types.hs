{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , StandaloneDeriving
  #-}

module Data.Set.Class.Types where

-- | These types are used for @Monoid@ and @Commutative@ instances for sets.

newtype Union a = Union {fromUnion :: a}

newtype Intersection a = Intersection {fromIntersection :: a}
