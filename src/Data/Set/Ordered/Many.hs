{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , DeriveFunctor
  , DeriveFoldable
  , DeriveTraversable
  , FlexibleContexts
  , MultiParamTypeClasses
  , OverloadedLists
  , TupleSections
  #-}

module Data.Set.Ordered.Many where

import Data.Mergeable
import Data.List as List hiding (delete)
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

instance (Arbitrary a, Ord a) => Arbitrary (OMSet a) where
  arbitrary = OMSet <$> sized go
    where
      go s = evalStateT (Vector.replicateM s go') Nothing
      go' :: ( Ord a
             , Arbitrary a
             ) => StateT (Maybe a) Gen a
      go' = do
        mprev <- get
        x <- liftBase $ maybe arbitrary (\p -> arbitrary `suchThat` (>= p)) mprev
        put $ Just x
        return x

-- * Operators

(\\) :: Eq a => OMSet a -> OMSet a -> OMSet a
(\\) = difference

-- * Query

-- | /O(1)/
null :: Eq a => OMSet a -> Bool
null (OMSet xs) = Vector.null xs

-- | /O(n)/
size :: OMSet a -> Int
size (OMSet xs) = Vector.length xs

-- | /O(n)/
member :: Eq a => a -> OMSet a -> Bool
member x (OMSet xs) = Vector.elem x xs

-- | /O(n)/
notMember :: Eq a => a -> OMSet a -> Bool
notMember x = not . member x

-- | /O(n)/
lookup :: Eq a => a -> OMSet a -> Maybe a
lookup x (OMSet xs) = lookup' x xs
  where
    lookup' _ [] = Nothing
    lookup' x' yss
      | Vector.null yss = Nothing
      | x == Vector.head yss = Just (Vector.head yss)
      | otherwise = lookup' x' (Vector.tail yss)

-- | /O(n*m)/
isSubsetOf :: Eq a => OMSet a -> OMSet a -> Bool
isSubsetOf (OMSet xs) (OMSet ys) = Vector.foldr go True xs
  where
    go x b | Vector.elem x ys = b
           | otherwise        = False

-- | /O(n*(m^3))/
isProperSubsetOf :: Eq a => OMSet a -> OMSet a -> Bool
isProperSubsetOf (OMSet xs) (OMSet ys) = fst (List.foldr go (True,ys) xs)
  where
    go _ (False,soFar) = (False,soFar)
    go _ (_,[]) = (False,[])
    go x (b,soFar)
      | Vector.elem x soFar = (b, Vector.filter (/= x) soFar)
      | otherwise = (False, soFar)

-- * Construction

-- | /O(1)/
empty :: OMSet a
empty = OMSet []

-- | /O(1)/
singleton :: a -> OMSet a
singleton x = OMSet [x]

-- | /O(n)/
insert :: Ord a => a -> OMSet a -> OMSet a
insert x (OMSet xs) = OMSet (insert' x xs)
  where
    insert' x' v
      | Vector.null v = [x']
      | x' <= Vector.head v = x' `Vector.cons` v
      | otherwise = (Vector.head v) `Vector.cons` (insert' x' (Vector.tail v))

-- | /O(n)/
delete :: Eq a => a -> OMSet a -> OMSet a
delete x (OMSet xs) = OMSet (Vector.filter (== x) xs)

-- * Combine

-- | /O(min n m)/
union :: Ord a => OMSet a -> OMSet a -> OMSet a
union (OMSet xs') (OMSet ys') = OMSet (go xs' ys')
  where
    go xss yss = case (xss Vector.!? 0, yss Vector.!? 0) of
      (Nothing,_) -> yss
      (_,Nothing) -> xss
      (Just x, Just y)
        | x == y -> Vector.cons x (go (Vector.drop 1 xss) yss)
        | otherwise -> Vector.cons y (go xss (Vector.drop 1 yss))

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
