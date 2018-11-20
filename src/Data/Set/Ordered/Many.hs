{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , DeriveFunctor
  , DeriveFoldable
  , DeriveTraversable
  , FlexibleContexts
  , MultiParamTypeClasses
  , OverloadedLists
  , TupleSections
  , FlexibleInstances
  #-}

module Data.Set.Ordered.Many where

import Data.Mergeable
import Data.Foldable as Fold
import Data.Traversable
import Data.Maybe (fromJust, isJust, mapMaybe)
import Data.Vector (Vector, (!?))
import qualified Data.Vector as Vector
import Control.Applicative
import Control.Monad.Fix
import Control.Monad.State
import Control.Monad.Base

import Test.QuickCheck
import System.IO.Unsafe (unsafePerformIO)


-- | Ordered sets with duplicate elements.
newtype OMSet a = OMSet {unOMSet :: Vector a}
  deriving ( Eq
           , Show
           , Functor
           , Applicative
           , Monad
           , Fold.Foldable
           , Traversable
           -- , MonadFix
           )

instance Mergeable OMSet where
  mergeMap f (OMSet xs) = mergeMap f xs

instance MonadBase Gen Gen where
  liftBase = id

instance Arbitrary (OMSet Int) where -- (Arbitrary a, Ord a, Show a) => Arbitrary (OMSet a) where
  arbitrary = OMSet <$> sized go
    where
      go s = let xs = [1..s]
             in  pure xs -- unsafePerformIO (print xs) `seq` (pure xs)
      -- go s = do x <- arbitrary
      --           xs <- go' s x
      --           let xss = Vector.cons x xs
      --           unsafePerformIO (print xss) `seq` (pure xss)
      -- go' :: (Ord a, Arbitrary a) => Int -> a -> Gen (Vector a)
      -- go' 0 _ = pure []
      -- go' s' prev = do
      --   next <- arbitrary `suchThat` (>= prev)
      --   (Vector.cons next) <$> go' (s' - 1) next

-- * Operators

(\\) :: Eq a => OMSet a -> OMSet a -> OMSet a
(\\) = difference

-- * Query

null :: Eq a => OMSet a -> Bool
null (OMSet xs) = Vector.null xs

size :: OMSet a -> Int
size (OMSet xs) = Vector.length xs

member :: Eq a => a -> OMSet a -> Bool
member x (OMSet xs) = Vector.elem x xs

notMember :: Eq a => a -> OMSet a -> Bool
notMember x = not . member x

lookup :: Eq a => a -> OMSet a -> Maybe a
lookup x (OMSet xs) = Vector.find (== x) xs

isSubsetOf :: Eq a => OMSet a -> OMSet a -> Bool
isSubsetOf (OMSet xs) (OMSet ys) = Vector.all (`Vector.elem` ys) xs

isProperSubsetOf :: Eq a => OMSet a -> OMSet a -> Bool
isProperSubsetOf xs ys = xs /= ys && isSubsetOf xs ys

-- * Construction

-- | /O(1)/
empty :: OMSet a
empty = OMSet Vector.empty

-- | /O(1)/
singleton :: a -> OMSet a
singleton = OMSet . Vector.singleton

-- | /O(n)/
insert :: Ord a => a -> OMSet a -> OMSet a
insert x (OMSet xs) =
  let (ps,ss) = Vector.span (<= x) xs
  in  OMSet (ps <> Vector.singleton x <> ss)

-- | /O(n)/
delete :: Eq a => a -> OMSet a -> OMSet a
delete x (OMSet xs) = OMSet (Vector.filter (/= x) xs)

-- * Combine

-- | /O(min n m)/
union :: Ord a => OMSet a -> OMSet a -> OMSet a
union xs ys = foldr insert xs ys

-- | /O(n*m)/
difference :: Eq a => OMSet a -> OMSet a -> OMSet a
difference (OMSet xs) (OMSet ys) = OMSet (Vector.foldr go [] xs)
 where
   go x soFar | Vector.elem x ys = soFar
              | otherwise        = Vector.cons x soFar

-- | /O(min(n,m))/ - Combines all elements of both
intersection :: Ord a => OMSet a -> OMSet a -> OMSet a
intersection (OMSet xs') (OMSet ys') = OMSet (go xs' ys')
  where
    go :: Ord a => Vector a -> Vector a -> Vector a
    go xss yss = case (,) <$> xss Vector.!? 0 <*> yss Vector.!? 0 of
      Nothing -> []
      Just (x,y)
        | x < y -> go xss (Vector.drop 1 yss)
        | x == y -> [x,y] <> go (Vector.drop 1 xss) (Vector.drop 1 yss)
        | otherwise -> go (Vector.drop 1 xss) yss

-- * Filter

-- | /O(n)/
filter :: (a -> Bool) -> OMSet a -> OMSet a
filter p (OMSet xs) = OMSet (Vector.filter p xs)

-- | /O(n)/
partition :: (a -> Bool) -> OMSet a -> (OMSet a, OMSet a)
partition p (OMSet xs) = let (l,r) = Vector.partition p xs in (OMSet l, OMSet r)

-- * Map

-- | /O(n)/
map :: (a -> b) -> OMSet a -> OMSet b
map f (OMSet xs) = OMSet (Vector.map f xs)

-- | /O(?)/
mapMaybe :: (a -> Maybe b) -> OMSet a -> OMSet b
mapMaybe f (OMSet xs) = OMSet (Vector.mapMaybe f xs)
