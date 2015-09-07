{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , DeriveFunctor
  , DeriveFoldable
  , DeriveTraversable
  , FlexibleContexts
  , MultiParamTypeClasses
  #-}

module Data.Set.Ordered.Many where

import Data.Mergeable
import Data.List as List hiding (delete)
import Data.Foldable as Fold
import Data.Traversable
import Data.Maybe (mapMaybe)
import Control.Applicative
import Control.Monad.Fix
import Control.Monad.State
import Control.Monad.Base

import Test.QuickCheck


-- | Ordered sets with duplicate elements.
newtype OMSet a = OMSet {unOMSet :: [a]}
  deriving ( Eq
           , Show
           , Functor
           , Applicative
           , Monad
           , Fold.Foldable
           , Traversable
           , MonadFix
           )

instance Mergeable OMSet where
  mergeMap f (OMSet xs) = mergeMap f xs


instance MonadBase Gen Gen where
  liftBase = id

instance (Arbitrary a, Ord a) => Arbitrary (OMSet a) where
  arbitrary = OMSet <$> sized go
    where
      go s = evalStateT (replicateM s go') Nothing
      go' :: ( MonadState (Maybe a) m
             , MonadBase Gen m
             , Ord a
             , Arbitrary a
             ) => m a
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
null (OMSet xs) = List.null xs

-- | /O(n)/
size :: OMSet a -> Int
size (OMSet xs) = List.length xs

-- | /O(n)/
member :: Eq a => a -> OMSet a -> Bool
member x (OMSet xs) = List.elem x xs

-- | /O(n)/
notMember :: Eq a => a -> OMSet a -> Bool
notMember x = not . member x

-- | /O(n)/
lookup :: Eq a => a -> OMSet a -> Maybe a
lookup x (OMSet xs) = lookup' x xs
  where
    lookup' _ [] = Nothing
    lookup' x' (y:ys) | x == y    = Just y
                      | otherwise = lookup' x' ys

-- | /O(n*m)/
isSubsetOf :: Eq a => OMSet a -> OMSet a -> Bool
isSubsetOf (OMSet xs) (OMSet ys) = List.foldr go True xs
  where
    go x b | List.elem x ys = b
           | otherwise      = False

-- | /O(n*(m^3))/
isProperSubsetOf :: Eq a => OMSet a -> OMSet a -> Bool
isProperSubsetOf (OMSet xs) (OMSet ys) = fst $ List.foldr go (True,ys) xs
  where
    go _ (False,soFar) = (False,soFar)
    go _ (_,[]) = (False,[])
    go x (b,soFar) = if List.elem x soFar
                     then (b,     List.filter (/= x) soFar)
                     else (False, soFar)

-- * Construction

-- | /O(1)/
empty :: OMSet a
empty = OMSet []

-- | /O(1)/
singleton :: a -> OMSet a
singleton x = OMSet [x]

-- | /O(n)/
insert :: Ord a => a -> OMSet a -> OMSet a
insert x (OMSet xs) = OMSet $ insert' x xs
  where
    insert' x' [] = [x']
    insert' x' (a:as) | x' <= a   = x': a:as
                      | otherwise = a : insert' x' as

-- | /O(n)/
delete :: Eq a => a -> OMSet a -> OMSet a
delete x (OMSet xs) = OMSet $ List.filter (== x) xs

-- * Combine

-- | /O(min n m)/
union :: Ord a => OMSet a -> OMSet a -> OMSet a
union (OMSet xs') (OMSet ys') = OMSet $ go xs' ys'
  where
    go [] ys = ys
    go xs [] = xs
    go (x:xs) (y:ys) | x <= y    = x : go xs (y:ys)
                     | otherwise = y : go (x:xs) ys

-- | /O(n*m)/
difference :: Eq a => OMSet a -> OMSet a -> OMSet a
difference (OMSet xs) (OMSet ys) = OMSet $ List.foldr go [] xs
 where
   go x soFar | List.elem x ys =   soFar
              | otherwise      = x:soFar

-- | /O(min(n,m))/ - Combines all elements of both
intersection :: Ord a => OMSet a -> OMSet a -> OMSet a
intersection (OMSet xs') (OMSet ys') = OMSet $ go xs' ys'
  where
    go [] _ = []
    go _ [] = []
    go (x:xs) (y:ys) | x < y = go (x:xs) ys
                     | x == y = x:x:go xs ys
                     | x > y = go xs (y:ys)

-- * Filter

-- | /O(n)/
filter :: (a -> Bool) -> OMSet a -> OMSet a
filter p (OMSet xs) = OMSet $ List.filter p xs

-- | /O(n)/
partition :: (a -> Bool) -> OMSet a -> (OMSet a, OMSet a)
partition p (OMSet xs) = let (l,r) = List.partition p xs in (OMSet l, OMSet r)

-- * Map

-- | /O(n)/
map :: (a -> b) -> OMSet a -> OMSet b
map f (OMSet xs) = OMSet $ List.map f xs

-- | /O(?)/
mapMaybe :: (a -> Maybe b) -> OMSet a -> OMSet b
mapMaybe f (OMSet xs) = OMSet $ Data.Maybe.mapMaybe f xs
