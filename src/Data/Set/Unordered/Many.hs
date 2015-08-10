{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , DeriveFunctor
  #-}

module Data.Set.Unordered.Many where

import Data.Mergeable
import Data.List as List hiding (delete)
import Data.Maybe (fromJust, isJust, mapMaybe)


-- | Unordered sets with duplicate elements. The semantics for "unordering" is based on the idea
-- that we will not know what order the elements are in at any point, and we
-- are free to re-order elements in any way.
--
-- Most binary functions are algorithmically heavier on the right arguments.

-- | Pronounced "Unordered Many Set"
newtype UMSet a = UMSet {unUMSet :: [a]}
  deriving (Functor)

instance Mergeable UMSet where
  mergeMap f (UMSet xs) = mergeMap f xs

-- * Operators

(\\) :: Eq a => UMSet a -> UMSet a -> UMSet a
(\\) = difference

-- * Query

-- | /O(1)/
null :: Eq a => UMSet a -> Bool
null (UMSet xs) = List.null xs

-- | /O(n)/
size :: UMSet a -> Int
size (UMSet xs) = List.length xs

-- | /O(n)/
member :: Eq a => a -> UMSet a -> Bool
member x (UMSet xs) = List.elem x xs

-- | /O(n)/
notMember :: Eq a => a -> UMSet a -> Bool
notMember x = not . member x

-- | /O(n)/
lookup :: Eq a => a -> UMSet a -> Maybe a
lookup x (UMSet xs) = lookup' x xs
  where
    lookup' _ [] = Nothing
    lookup' x (y:ys) | x == y    = Just y
                     | otherwise = lookup' x ys

-- | /O(n*m)/
isSubsetOf :: Eq a => UMSet a -> UMSet a -> Bool
isSubsetOf (UMSet xs) (UMSet ys) = foldr go True xs
  where
    go x b | List.elem x ys = b
           | otherwise      = False

-- | /O(n*(m^3))/
isProperSubsetOf :: Eq a => UMSet a -> UMSet a -> Bool
isProperSubsetOf (UMSet xs) (UMSet ys) = fst $ foldr go (True,ys) xs
  where
    go _ (False,soFar) = (False,soFar)
    go _ (_,[]) = (False,[])
    go x (b,soFar) = if List.elem x soFar
                     then (b,     List.filter (/= x) soFar)
                     else (False, soFar)

-- * Construction

-- | /O(1)/
empty :: UMSet a
empty = UMSet []

-- | /O(1)/
singleton :: a -> UMSet a
singleton x = UMSet [x]

-- | /O(1)/
insert :: a -> UMSet a -> UMSet a
insert x (UMSet xs) = UMSet $ x:xs

-- | /O(n)/
delete :: Eq a => a -> UMSet a -> UMSet a
delete x (UMSet xs) = UMSet $ List.filter (== x) xs

-- * Combine

-- | /O(n)/
union :: Eq a => UMSet a -> UMSet a -> UMSet a
union (UMSet xs) (UMSet ys) = UMSet $ xs ++ ys

-- | /O(n*m)/
difference :: Eq a => UMSet a -> UMSet a -> UMSet a
difference (UMSet xs) (UMSet ys) = UMSet $ foldr go [] xs
  where
    go x soFar | List.elem x ys =   soFar
               | otherwise      = x:soFar

-- | /O(n*(m^4))/ - Combines all elements of both
intersection :: Eq a => UMSet a -> UMSet a -> UMSet a
intersection (UMSet xs) (UMSet ys) = UMSet $ fst $ foldr go ([],ys) xs
  where
    go :: Eq a => a -> ([a],[a]) -> ([a],[a])
    go x (soFar,whatsLeft) | List.elem x whatsLeft =
                               ( soFar ++ List.filter (== x) whatsLeft
                               , List.filter (/= x) whatsLeft )
                           | otherwise =
                               ( soFar
                               , whatsLeft )

-- * Filter

-- | /O(n)/
filter :: (a -> Bool) -> UMSet a -> UMSet a
filter p (UMSet xs) = UMSet $ List.filter p xs

-- | /O(n)/
partition :: (a -> Bool) -> UMSet a -> (UMSet a, UMSet a)
partition p (UMSet xs) = let (l,r) = List.partition p xs in (UMSet l, UMSet r)

-- * Map

-- | /O(n)/
map :: (a -> b) -> UMSet a -> UMSet b
map f (UMSet xs) = UMSet $ List.map f xs

-- | /O(?)/
mapMaybe :: (a -> Maybe b) -> UMSet a -> UMSet b
mapMaybe f (UMSet xs) = UMSet $ Data.Maybe.mapMaybe f xs
