{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , NoImplicitPrelude
  #-}


-- | Orient the ordering of your set by a different index, by first supplying a
-- function @(a -> k)@ to weigh each element. This module simply leverages
-- @Data.Map@, and does not use a novel data type.
--
-- Note: This data type can only have one element per distinguished weight. For
-- oriented multisets, use @Data.Set.Ordered.Many.With.SetsWith@.

module Data.Set.Ordered.Unique.With where

import Prelude ( Show, String, Eq, Ord, Bool, Int, Maybe
               , fmap, not, fst, snd, zip, (.), ($), foldr)
import qualified Data.Map as Map
import qualified Data.List as List
import Data.Maybe (isJust)
import qualified Data.Foldable as Fold
--import Data.Functor.Invariant
import Control.Applicative ((<$>))
import Data.Monoid (Monoid)


newtype SetWith k a = SetWith {unSetWith :: (a -> k, Map.Map k a)}
  deriving (Monoid)

--instance Invariant (SetWith k) where
--  invmap = map

instance Fold.Foldable (SetWith k) where
  foldr = Data.Set.Ordered.Unique.With.foldr


-- * Operators

(\\) :: Ord k => SetWith k a -> SetWith k a -> SetWith k a
(SetWith (f,xs)) \\ (SetWith (_,ys)) = SetWith (f, Map.difference xs ys)

-- * Query

null :: SetWith k a -> Bool
null (SetWith (_,xs)) = Map.null xs

size :: SetWith k a -> Int
size (SetWith (_,xs)) = Map.size xs

member :: Ord k => a -> SetWith k a -> Bool
member x (SetWith (f,xs)) = Map.member (f x) xs

notMember :: Ord k => a -> SetWith k a -> Bool
notMember x = not . member x

lookupLT :: Ord k => a -> SetWith k a -> Maybe a
lookupLT x (SetWith (f,xs)) = snd <$> Map.lookupLT (f x) xs

lookupGT :: Ord k => a -> SetWith k a -> Maybe a
lookupGT x (SetWith (f,xs)) = snd <$> Map.lookupGT (f x) xs

lookupLE :: Ord k => a -> SetWith k a -> Maybe a
lookupLE x (SetWith (f,xs)) = snd <$> Map.lookupLE (f x) xs

lookupGE :: Ord k => a -> SetWith k a -> Maybe a
lookupGE x (SetWith (f,xs)) = snd <$> Map.lookupGE (f x) xs

isSubsetOf :: (Eq a, Ord k) => SetWith k a -> SetWith k a -> Bool
isSubsetOf (SetWith (_,xs)) (SetWith (_,ys)) = Map.isSubmapOf xs ys

isProperSubsetOf :: (Eq a, Ord k) => SetWith k a -> SetWith k a -> Bool
isProperSubsetOf (SetWith (_,xs)) (SetWith (_,ys)) = Map.isProperSubmapOf xs ys

-- * Construction

empty :: (a -> k) -> SetWith k a
empty f = SetWith (f, Map.empty)

singleton :: Ord k => (a -> k) -> a -> SetWith k a
singleton f x = insert x (empty f)

insert :: Ord k => a -> SetWith k a -> SetWith k a
insert x (SetWith (f,xs)) = SetWith (f, Map.insert (f x) x xs)

delete :: Ord k => a -> SetWith k a -> SetWith k a
delete x (SetWith (f,xs)) = SetWith (f, Map.delete (f x) xs)

-- * Combine

union :: Ord k => SetWith k a -> SetWith k a -> SetWith k a
union (SetWith (f,xs)) (SetWith (_,ys)) = SetWith (f, Map.union xs ys)

unions :: Ord k => (a -> k) -> [SetWith k a] -> SetWith k a
unions f = List.foldl' union $ empty f

difference :: Ord k => SetWith k a -> SetWith k a -> SetWith k a
difference (SetWith (f,xs)) (SetWith (_,ys)) = SetWith (f, Map.difference xs ys)

intersection :: Ord k => SetWith k a -> SetWith k a -> SetWith k a
intersection (SetWith (f,xs)) (SetWith (_,ys)) = SetWith (f, Map.intersection xs ys)

-- * Filter

filter :: (a -> Bool) -> SetWith k a -> SetWith k a
filter p (SetWith (f,xs)) = SetWith (f, Map.filter p xs)

partition :: (a -> Bool) -> SetWith k a -> (SetWith k a, SetWith k a)
partition p (SetWith (f,xs)) = let zs = Map.partition p xs
                               in (SetWith (f, fst zs), SetWith (f, snd zs))

split :: Ord k => a -> SetWith k a -> (SetWith k a, SetWith k a)
split x (SetWith (f,xs)) = let zs = Map.split (f x) xs
                           in (SetWith (f, fst zs), SetWith (f, snd zs))

splitMember :: Ord k => a -> SetWith k a -> (SetWith k a, Bool, SetWith k a)
splitMember x (SetWith (f,xs)) = let (l,b,r) = Map.splitLookup (f x) xs
                                 in (SetWith (f,l), isJust b, SetWith (f,r))

splitRoot :: SetWith k a -> [SetWith k a]
splitRoot (SetWith (f,xs)) = let xss = Map.splitRoot xs
                             in fmap (\a -> SetWith (f,a)) xss

-- * Indexed

lookupIndex :: Ord k => a -> SetWith k a -> Maybe Int
lookupIndex x (SetWith (f,xs)) = Map.lookupIndex (f x) xs

findIndex :: Ord k => a -> SetWith k a -> Int
findIndex x (SetWith (f,xs)) = Map.findIndex (f x) xs

elemAt :: Int -> SetWith k a -> a
elemAt i (SetWith (_,xs)) = snd $ Map.elemAt i xs

deleteAt :: Int -> SetWith k a -> SetWith k a
deleteAt i (SetWith (f,xs)) = SetWith (f, Map.deleteAt i xs)

-- * Map

map :: (a -> b) -> (b -> a) -> SetWith k a -> SetWith k b
map f g (SetWith (p,xs)) = SetWith (p . g, Map.map f xs)

mapMaybe :: (a -> Maybe b) -> (b -> a) -> SetWith k a -> SetWith k b
mapMaybe f g (SetWith (p,xs)) = SetWith (p . g, Map.mapMaybe f xs)

-- * Folds

foldr :: (a -> b -> b) -> b -> SetWith k a -> b
foldr f acc (SetWith (_,xs)) = Map.foldr f acc xs

foldl :: (b -> a -> b) -> b -> SetWith k a -> b
foldl f acc (SetWith (_,xs)) = Map.foldl f acc xs

-- ** Strict Folds

foldr' :: (a -> b -> b) -> b -> SetWith k a -> b
foldr' f acc (SetWith (_,xs)) = Map.foldr' f acc xs

foldl' :: (b -> a -> b) -> b -> SetWith k a -> b
foldl' f acc (SetWith (_,xs)) = Map.foldl' f acc xs

-- ** Legacy Fold

fold :: (a -> b -> b) -> b -> SetWith k a -> b
fold f acc (SetWith (_,xs)) = Map.fold f acc xs

-- * Min/Max

findMin :: SetWith k a -> a
findMin = snd . Map.findMin . snd . unSetWith

findMax :: SetWith k a -> a
findMax = snd . Map.findMax . snd . unSetWith

deleteMin :: SetWith k a -> SetWith k a
deleteMin (SetWith (f,xs)) = SetWith (f, Map.deleteMin xs)

deleteMax :: SetWith k a -> SetWith k a
deleteMax (SetWith (f,xs)) = SetWith (f, Map.deleteMax xs)

deleteFindMin :: SetWith k a -> (a, SetWith k a)
deleteFindMin (SetWith (f,xs)) = let ((_,l),zs) = Map.deleteFindMin xs
                                 in (l, SetWith (f,zs))

deleteFindMax :: SetWith k a -> (a, SetWith k a)
deleteFindMax (SetWith (f,xs)) = let ((_,l),zs) = Map.deleteFindMax xs
                                 in (l, SetWith (f,zs))

minView :: SetWith k a -> Maybe (a, SetWith k a)
minView (SetWith (f,xs)) = (\(l,a) -> (l, SetWith (f,a))) <$> Map.minView xs

maxView :: SetWith k a -> Maybe (a, SetWith k a)
maxView (SetWith (f,xs)) = (\(l,a) -> (l, SetWith (f,a))) <$> Map.maxView xs

-- * Conversion

elems :: SetWith k a -> [a]
elems (SetWith (_,xs)) = Map.elems xs

toList :: SetWith k a -> (a -> k, [a])
toList (SetWith (f,xs)) = (f, Map.elems xs)

fromList :: (Ord k, Fold.Foldable f) => (a -> k) -> f a -> SetWith k a
fromList f = Fold.foldr insert $ empty f

-- * Ordered List

toAscList :: SetWith k a -> [a]
toAscList (SetWith (_,xs)) = snd <$> Map.toAscList xs

toDescList :: SetWith k a -> [a]
toDescList (SetWith (_,xs)) = snd <$> Map.toDescList xs

fromAscList :: Eq k => (a -> k) -> [a] -> SetWith k a
fromAscList f xs = SetWith (f, Map.fromAscList $ (f <$> xs) `zip` xs)

fromDistinctAscList :: (a -> k) -> [a] -> SetWith k a
fromDistinctAscList f xs = SetWith (f, Map.fromDistinctAscList $ (f <$> xs) `zip` xs)

-- * Debugging

showTree :: (Show k, Show a) => SetWith k a -> String
showTree (SetWith (_,xs)) = Map.showTree xs

showTreeWith :: (k -> a -> String) -> Bool -> Bool -> SetWith k a -> String
showTreeWith f a b (SetWith (_,xs)) = Map.showTreeWith f a b xs
