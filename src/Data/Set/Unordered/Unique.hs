{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , DeriveFunctor
  , FlexibleContexts
  , MultiParamTypeClasses
  #-}

-- | Unique, unordered sets. The semantics for "unordering" is based on the idea
-- that we will not know what order the elements are in at any point, and we
-- are free to re-order elements in any way.

module Data.Set.Unordered.Unique where

import Data.Mergeable
import Data.List as List
import Data.Maybe (fromJust, isJust, mapMaybe)
import Control.Monad.State
import Control.Monad.Base
import Control.Applicative

import Test.QuickCheck


-- | Pronounced "Unordered Unique Set"
newtype UUSet a = UUSet {unUUSet :: [a]}
  deriving (Functor, Show)

instance Mergeable UUSet where
  mergeMap f (UUSet xs) = mergeMap f xs

instance Eq a => Eq (UUSet a) where
  (UUSet xs) == (UUSet ys) = case foldr go (Just xs) ys of
    Just [] -> True
    _       -> False
    where
      go _ Nothing = Nothing
      go _ (Just []) = Nothing
      go y (Just xs') | y `elem` xs' = Just $ List.delete y xs'
                      | otherwise = Nothing

instance MonadBase Gen Gen where
  liftBase = id

instance (Arbitrary a, Eq a) => Arbitrary (UUSet a) where
  arbitrary = UUSet <$> sized go
    where go s = evalStateT (replicateM s go') []
          go' :: ( MonadState [a] m
                , MonadBase Gen m
                , Eq a
                , Arbitrary a
                ) => m a
          go' = do
            soFar <- get
            x <- liftBase $ arbitrary `suchThat` (`notElem` soFar)
            put $ x:soFar
            return x

-- * Operators

(\\) :: Eq a => UUSet a -> UUSet a -> UUSet a
(\\) = difference

-- * Query

-- | /O(1)/
null :: Eq a => UUSet a -> Bool
null (UUSet xs) = List.null xs

-- | /O(n)/
size :: UUSet a -> Int
size (UUSet xs) = List.length xs

-- | /O(n)/
member :: Eq a => a -> UUSet a -> Bool
member x (UUSet xs) = List.elem x xs

-- | /O(n)/
notMember :: Eq a => a -> UUSet a -> Bool
notMember x = not . member x

-- | /O(n)/
lookup :: Eq a => a -> UUSet a -> Maybe a
lookup x (UUSet xs) = lookup' x xs
  where
    lookup' _ [] = Nothing
    lookup' x (y:ys) | x == y    = Just y
                     | otherwise = lookup' x ys

-- | /O(n*m)/
isSubsetOf :: Eq a => UUSet a -> UUSet a -> Bool
isSubsetOf (UUSet xs) (UUSet ys) = foldr go True xs
  where
    go x b | List.elem x ys = b
           | otherwise      = False

-- | /O(n*(m^2))/
isProperSubsetOf :: Eq a => UUSet a -> UUSet a -> Bool
isProperSubsetOf (UUSet xs) (UUSet ys) = fst $ foldr go (True,ys) xs
  where
    go _ (False,xs) = (False,xs)
    go _ (_,[]) = (False,[])
    go x (b,soFar) = let midx = List.elemIndex x soFar in
      if isJust midx then (b,     deleteAt (fromJust midx) soFar)
                     else (False, soFar)

    deleteAt n xs = List.take n xs ++ List.drop (n+1) xs

-- * Construction

-- | /O(1)/
empty :: UUSet a
empty = UUSet []

-- | /O(1)/
singleton :: a -> UUSet a
singleton x = UUSet [x]

-- | /O(n)/
insert :: Eq a => a -> UUSet a -> UUSet a
insert x (UUSet xs) = UUSet $ insert' x xs
  where
    insert' x [] = [x]
    insert' x (y:ys) | x == y    = y:ys
                     | otherwise = y:insert' x ys

-- | /O(n)/
delete :: Eq a => a -> UUSet a -> UUSet a
delete x (UUSet xs) = UUSet $ delete' x xs
  where
    delete' x [] = []
    delete' x (y:ys) | x == y    =   ys
                     | otherwise = y:delete' x ys

-- * Combine

-- | /O(n*m)/
union :: Eq a => UUSet a -> UUSet a -> UUSet a
union (UUSet xs) (UUSet ys) = UUSet $ foldr go xs ys
  where
    go y soFar | List.elem y soFar =   soFar
               | otherwise         = y:soFar

-- | /O(n*m)/
difference :: Eq a => UUSet a -> UUSet a -> UUSet a
difference (UUSet xs) (UUSet ys) = UUSet $ foldr go [] xs
  where
    go x soFar | List.elem x ys =   soFar
               | otherwise      = x:soFar

-- | /O(n*m)/
intersection :: Eq a => UUSet a -> UUSet a -> UUSet a
intersection (UUSet xs) (UUSet ys) = UUSet $ foldr go [] xs
  where
    go x soFar | List.elem x ys = x:soFar
               | otherwise      =   soFar

-- * Filter

-- | /O(n)/
filter :: (a -> Bool) -> UUSet a -> UUSet a
filter p (UUSet xs) = UUSet $ List.filter p xs

-- | /O(n)/ - Guaranteed to be disjoint
partition :: (a -> Bool) -> UUSet a -> (UUSet a, UUSet a)
partition p (UUSet xs) = let (l,r) = List.partition p xs in (UUSet l, UUSet r)

-- * Map

-- | /O(n)/
map :: (a -> b) -> UUSet a -> UUSet b
map f (UUSet xs) = UUSet $ List.map f xs

-- | /O(?)/
mapMaybe :: (a -> Maybe b) -> UUSet a -> UUSet b
mapMaybe f (UUSet xs) = UUSet $ Data.Maybe.mapMaybe f xs
