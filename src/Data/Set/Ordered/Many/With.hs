{-# LANGUAGE
    NoImplicitPrelude
  , GeneralizedNewtypeDeriving
  #-}

module Data.Set.Ordered.Many.With where

import Prelude (($), (.), Eq (..), Ord (..), Maybe (..)
               , Bool (..), (&&), (||), and, not, Functor (..), Int)
import qualified Data.Set.Class as Sets
import qualified Data.Set.Ordered.Many as OM
import qualified Data.Set.Ordered.Unique.With as OU
import qualified Data.Map as Map
import qualified Data.Witherable as Wither

import Data.Monoid
import Data.Functor.Invariant
import Control.Applicative hiding (empty)


newtype SetsWith k c a = SetsWith {unSetsWith :: (a -> k, Map.Map k (c a))}

instance Functor c => Invariant (SetsWith k c) where
  invmap = map

-- instance Fold.Foldable (SetWith k) where
--   foldr = Data.Set.Ordered.Unique.With.foldr


-- * Operators

(\\) :: ( Ord k
        , Sets.HasDifference (c a)
        ) => SetsWith k c a -> SetsWith k c a -> SetsWith k c a
(SetsWith (f,xs)) \\ (SetsWith (_,ys)) = SetsWith
  (f, Map.unionWith Sets.difference xs (ys `Map.intersection` xs))

-- * Query

null :: SetsWith k c a -> Bool
null (SetsWith (_,xs)) = Map.null xs

size :: SetsWith k c a -> Int
size (SetsWith (_,xs)) = Map.size xs

member :: Ord k => a -> SetsWith k c a -> Bool
member x (SetsWith (f,xs)) = Map.member (f x) xs -- Depends on eager pruning

notMember :: Ord k => a -> SetsWith k c a -> Bool
notMember x = not . member x

-- lookupLT :: Ord k => a -> SetsWith k c a -> Maybe a
-- lookupLT x (SetWith (f,xs)) = snd <$> Map.lookupLT (f x) xs
--
-- lookupGT :: Ord k => a -> SetWith k a -> Maybe a
-- lookupGT x (SetWith (f,xs)) = snd <$> Map.lookupGT (f x) xs
--
-- lookupLE :: Ord k => a -> SetWith k a -> Maybe a
-- lookupLE x (SetWith (f,xs)) = snd <$> Map.lookupLE (f x) xs
--
-- lookupGE :: Ord k => a -> SetWith k a -> Maybe a
-- lookupGE x (SetWith (f,xs)) = snd <$> Map.lookupGE (f x) xs

isSubsetOf :: ( Ord k
              , Eq (c a)
              , Sets.CanBeSubset (c a)
              ) => SetsWith k c a -> SetsWith k c a -> Bool
isSubsetOf (SetsWith (_,xs)) (SetsWith (_,ys)) = Map.isSubmapOf xs ys &&
  and (getZipList $ (Sets.isSubsetOf) <$> (ZipList $ Map.elems $ xs `Map.intersection` ys)
                                      <*> (ZipList $ Map.elems $ ys `Map.intersection` xs))

isProperSubsetOf :: ( Ord k
                    , Eq (c a)
                    , Sets.CanBeSubset (c a)
                    ) => SetsWith k c a -> SetsWith k c a -> Bool
isProperSubsetOf xss@(SetsWith (_,xs)) yss@(SetsWith (_,ys)) =
  xss `isSubsetOf` yss && size xss < size yss

-- * Construction

empty :: (a -> k) -> SetsWith k c a
empty f = SetsWith (f, Map.empty)

singleton :: ( Ord k
             , Sets.HasUnion (c a)
             , Sets.HasSingleton a (c a)
             ) => (a -> k) -> a -> SetsWith k c a
singleton f x = insert x (empty f)

insert :: ( Ord k
          , Sets.HasUnion (c a)
          , Sets.HasSingleton a (c a)
          ) => a -> SetsWith k c a -> SetsWith k c a
insert x (SetsWith (f,xs)) = SetsWith
  (f, Map.insertWith Sets.union (f x) (Sets.singleton x) xs)

delete :: ( Ord k
          , Eq (c a)
          , Sets.HasEmpty (c a)
          , Sets.HasDelete a (c a)
          ) => a -> SetsWith k c a -> SetsWith k c a
delete x (SetsWith (f,xs)) =
  case Map.lookup (f x) xs of
    Just set -> if Sets.delete x set == Sets.empty
                then SetsWith (f, Map.delete (f x) xs)
                else SetsWith (f, Map.update (Just . Sets.delete x) (f x) xs)
    Nothing -> SetsWith (f,xs)

-- * Combine

union :: ( Ord k
         , Sets.HasUnion (c a)
         ) => SetsWith k c a -> SetsWith k c a -> SetsWith k c a
union (SetsWith (f,xs)) (SetsWith (_,ys)) = SetsWith
  (f, Map.unionWith Sets.union xs ys)

difference :: ( Ord k
              , Eq (c a)
              , Sets.HasEmpty (c a)
              , Sets.HasDifference (c a)
              ) => SetsWith k c a -> SetsWith k c a -> SetsWith k c a
difference (SetsWith (f,xs)) (SetsWith (_,ys)) = SetsWith
  (f, Map.filter (/= Sets.empty) $ Map.unionWith Sets.difference
        xs (ys `Map.intersection` xs))

intersection :: ( Ord k
                , Eq (c a)
                , Sets.HasEmpty (c a)
                , Sets.HasIntersection (c a)
                ) => SetsWith k c a -> SetsWith k c a -> SetsWith k c a
intersection (SetsWith (f,xs)) (SetsWith (_,ys)) = SetsWith
  (f, Map.filter (/= Sets.empty) $ Map.intersectionWith Sets.intersection xs ys)

-- -- * Filter

filter :: ( Eq (c a)
          , Sets.HasEmpty (c a)
          , Wither.Witherable c
          ) => (a -> Bool) -> SetsWith k c a -> SetsWith k c a
filter p (SetsWith (f,xs)) = SetsWith (f, Map.filter (/= Sets.empty) $
  Map.map (Wither.filter p) xs)

-- partition :: (a -> Bool) -> SetWith k a -> (SetWith k a, SetWith k a)
-- partition p (SetWith (f,xs)) = let zs = Map.partition p xs
--                                in (SetWith (f, fst zs), SetWith (f, snd zs))
--
-- split :: Ord k => a -> SetWith k a -> (SetWith k a, SetWith k a)
-- split x (SetWith (f,xs)) = let zs = Map.split (f x) xs
--                            in (SetWith (f, fst zs), SetWith (f, snd zs))
--
-- splitMember :: Ord k => a -> SetWith k a -> (SetWith k a, Bool, SetWith k a)
-- splitMember x (SetWith (f,xs)) = let (l,b,r) = Map.splitLookup (f x) xs
--                                  in (SetWith (f,l), isJust b, SetWith (f,r))
--
-- splitRoot :: Ord k => SetWith k a -> [SetWith k a]
-- splitRoot (SetWith (f,xs)) = let xss = Map.splitRoot xs
--                              in fmap (\a -> SetWith (f,a)) xss

-- -- * Indexed

-- lookupIndex :: Ord k => a -> SetsWith k c a -> Maybe Int
-- lookupIndex x (SetWith (f,xs)) = Map.lookupIndex (f x) xs
--
-- findIndex :: Ord k => a -> SetWith k a -> Int
-- findIndex x (SetWith (f,xs)) = Map.findIndex (f x) xs
--
-- elemAt :: Int -> SetWith k a -> a
-- elemAt i (SetWith (_,xs)) = snd $ Map.elemAt i xs
--
-- deleteAt :: Int -> SetWith k a -> SetWith k a
-- deleteAt i (SetWith (f,xs)) = SetWith (f, Map.deleteAt i xs)

-- -- * Map
--
map :: Functor c => (a -> b) -> (b -> a) -> SetsWith k c a -> SetsWith k c b
map f g (SetsWith (p,xs)) = SetsWith (p . g, Map.map (fmap f) xs)

mapMaybe :: ( Eq (c b)
            , Sets.HasEmpty (c b)
            , Wither.Witherable c
            ) => (a -> Maybe b) -> (b -> a) -> SetsWith k c a -> SetsWith k c b
mapMaybe f g (SetsWith (p,xs)) = SetsWith
  (p . g, Map.filter (/= Sets.empty) $ Map.map (Wither.mapMaybe f) xs)

-- -- * Folds
--
-- foldr :: (a -> b -> b) -> b -> SetWith k a -> b
-- foldr f acc (SetWith (_,xs)) = Map.foldr f acc xs
--
-- foldl :: (b -> a -> b) -> b -> SetWith k a -> b
-- foldl f acc (SetWith (_,xs)) = Map.foldl f acc xs
--
-- -- ** Strict Folds
--
-- foldr' :: (a -> b -> b) -> b -> SetWith k a -> b
-- foldr' f acc (SetWith (_,xs)) = Map.foldr' f acc xs
--
-- foldl' :: (b -> a -> b) -> b -> SetWith k a -> b
-- foldl' f acc (SetWith (_,xs)) = Map.foldl' f acc xs
--
-- -- ** Legacy Fold
--
-- fold :: (a -> b -> b) -> b -> SetWith k a -> b
-- fold f acc (SetWith (_,xs)) = Map.fold f acc xs
--
-- -- * Min/Max
--
-- findMin :: SetWith k a -> a
-- findMin = snd . Map.findMin . snd . unSetWith
--
-- findMax :: SetWith k a -> a
-- findMax = snd . Map.findMax . snd . unSetWith
--
-- deleteMin :: SetWith k a -> SetWith k a
-- deleteMin (SetWith (f,xs)) = SetWith (f, Map.deleteMin xs)
--
-- deleteMax :: SetWith k a -> SetWith k a
-- deleteMax (SetWith (f,xs)) = SetWith (f, Map.deleteMax xs)
--
-- deleteFindMin :: SetWith k a -> (a, SetWith k a)
-- deleteFindMin (SetWith (f,xs)) = let ((_,l),zs) = Map.deleteFindMin xs
--                                  in (l, SetWith (f,zs))
--
-- deleteFindMax :: SetWith k a -> (a, SetWith k a)
-- deleteFindMax (SetWith (f,xs)) = let ((_,l),zs) = Map.deleteFindMax xs
--                                  in (l, SetWith (f,zs))
--
-- minView :: SetWith k a -> Maybe (a, SetWith k a)
-- minView (SetWith (f,xs)) = (\(l,a) -> (l, SetWith (f,a))) <$> Map.minView xs
--
-- maxView :: SetWith k a -> Maybe (a, SetWith k a)
-- maxView (SetWith (f,xs)) = (\(l,a) -> (l, SetWith (f,a))) <$> Map.maxView xs
--
-- -- * Conversion
--
-- elems :: SetWith k a -> [a]
-- elems (SetWith (_,xs)) = Map.elems xs
--
-- toList :: SetWith k a -> (a -> k, [a])
-- toList (SetWith (f,xs)) = (f, Map.elems xs)
--
-- fromList :: Ord k => (a -> k) -> [a] -> SetWith k a
-- fromList f = List.foldr insert $ empty f
--
-- -- * Ordered List
--
-- toAscList :: SetWith k a -> [a]
-- toAscList (SetWith (_,xs)) = snd <$> Map.toAscList xs
--
-- toDescList :: SetWith k a -> [a]
-- toDescList (SetWith (_,xs)) = snd <$> Map.toDescList xs
--
-- fromAscList :: Eq k => (a -> k) -> [a] -> SetWith k a
-- fromAscList f xs = SetWith (f, Map.fromAscList $ (f <$> xs) `zip` xs)
--
-- fromDistinctAscList :: (a -> k) -> [a] -> SetWith k a
-- fromDistinctAscList f xs = SetWith (f, Map.fromDistinctAscList $ (f <$> xs) `zip` xs)
--
-- -- * Debugging
--
-- showTree :: (Show k, Show a) => SetWith k a -> String
-- showTree (SetWith (_,xs)) = Map.showTree xs
--
-- showTreeWith :: (Show k, Show a) => (k -> a -> String) -> Bool -> Bool -> SetWith k a -> String
-- showTreeWith f a b (SetWith (_,xs)) = Map.showTreeWith f a b xs
