module Data.Set.Ordered.Unique.Finite where

import qualified Data.Set as Set


newtype FiniteSet a = FiniteSet
  { unFiniteSet :: (Set.Set a, Set.Set a) }

-- * Operators

-- | /O(n+m)/
(\\) :: Ord a => FiniteSet a -> FiniteSet a -> FiniteSet a
(\\) = difference

-- * Query

-- | /O(1)/
null :: Eq a => FiniteSet a -> Bool
null (FiniteSet (_,xs)) = Set.null xs

-- | /O(1)/
size :: FiniteSet a -> Int
size (FiniteSet (_,xs)) = Set.size xs

-- | /O(log n)/
member :: Ord a => a -> FiniteSet a -> Bool
member x (FiniteSet (_,xs)) = Set.member x xs

-- | /O(log n)/
notMember :: Ord a => a -> FiniteSet a -> Bool
notMember x = not . member x

-- | /O(n+m+t1+t2)/
isSubsetOf :: Ord a => FiniteSet a -> FiniteSet a -> Bool
isSubsetOf (FiniteSet (t1,xs)) (FiniteSet (t2,ys)) =
  Set.isSubsetOf t1 t2 && Set.isSubsetOf xs ys

-- | /O(n+m+t1+t2)/
isProperSubsetOf :: Ord a => FiniteSet a -> FiniteSet a -> Bool
isProperSubsetOf (FiniteSet (t1,xs)) (FiniteSet (t2,ys)) =
  Set.isProperSubsetOf xs ys && Set.isSubsetOf t1 t2

-- * Construction

-- | /O(1)/
empty :: Set.Set a -> FiniteSet a
empty t = FiniteSet (t, Set.empty)

total :: FiniteSet a -> Set.Set a
total (FiniteSet (t,_)) = t

-- | /O(1)/
singleton :: Set.Set a -> a -> FiniteSet a
singleton t x = FiniteSet (t, Set.singleton x)

-- | /O(log n)/
insert :: Ord a => a -> FiniteSet a -> FiniteSet a
insert x (FiniteSet (t,xs)) = FiniteSet (t, Set.insert x xs)

-- | /O(log n)/
delete :: Ord a => a -> FiniteSet a -> FiniteSet a
delete x (FiniteSet (t,xs)) = FiniteSet (t, Set.delete x xs)

-- * Combine

-- | /O(n+m)/
union :: Ord a => FiniteSet a -> FiniteSet a -> FiniteSet a
union (FiniteSet (_,xs)) (FiniteSet (t,ys)) = FiniteSet (t, Set.union xs ys)

-- | /O(n+m)/
difference :: Ord a => FiniteSet a -> FiniteSet a -> FiniteSet a
difference (FiniteSet (_,xs)) (FiniteSet (t,ys)) = FiniteSet (t, Set.difference xs ys)

-- | /O(n+m)/
intersection :: Ord a => FiniteSet a -> FiniteSet a -> FiniteSet a
intersection (FiniteSet (_,xs)) (FiniteSet (t,ys)) = FiniteSet (t, Set.intersection xs ys)

-- | /O(n+t)
complement :: Ord a => FiniteSet a -> FiniteSet a
complement (FiniteSet (t,xs)) = FiniteSet (t, Set.difference t xs)

-- * Filter

-- | /O(n)/
filter :: (a -> Bool) -> FiniteSet a -> FiniteSet a
filter p (FiniteSet (t,xs)) = FiniteSet (t, Set.filter p xs)

-- | /O(n)/ - Guaranteed to be disjoint
partition :: (a -> Bool) -> FiniteSet a -> (FiniteSet a, FiniteSet a)
partition p (FiniteSet (t,xs)) = let (l,r) = Set.partition p xs
                                 in (FiniteSet (t,l), FiniteSet (t,r))

-- * Map

-- | /O(n)/
map :: Ord b => (a -> b) -> FiniteSet a -> FiniteSet b
map f (FiniteSet (t,xs)) = FiniteSet (Set.map f t, Set.map f xs)
