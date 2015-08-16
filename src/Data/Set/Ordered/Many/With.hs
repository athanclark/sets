{-# LANGUAGE
    NoImplicitPrelude
  , GeneralizedNewtypeDeriving
  #-}

module Data.Set.Ordered.Many.With where

import Prelude (($), (.), Eq (..), Ord (..), Maybe (..)
               , Bool (..), (&&), (||), and, not, Functor (..), Int
               , snd, zip, String (..), Show (..))
import qualified Data.Set.Class as Sets
import qualified Data.Set.Ordered.Many as OM
import qualified Data.Set.Ordered.Unique.With as OU
import qualified Data.Map as Map
import qualified Data.Witherable as Wither

import Data.Monoid
import Data.Functor.Invariant
import Data.Foldable as Fold
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

foldr :: Fold.Foldable c => (a -> b -> b) -> b -> SetsWith k c a -> b
foldr f acc (SetsWith (_,xs)) = Map.foldr go acc xs
  where
    go cs acc' = Fold.foldr f acc' cs

foldl :: Fold.Foldable c => (b -> a -> b) -> b -> SetsWith k c a -> b
foldl f acc (SetsWith (_,xs)) = Map.foldl go acc xs
  where
    go = Fold.foldl f

-- -- ** Strict Folds

foldr' :: Fold.Foldable c => (a -> b -> b) -> b -> SetsWith k c a -> b
foldr' f acc (SetsWith (_,xs)) = Map.foldr' go acc xs
  where
    go cs acc' = Fold.foldr' f acc' cs

foldl' :: Fold.Foldable c => (b -> a -> b) -> b -> SetsWith k c a -> b
foldl' f acc (SetsWith (_,xs)) = Map.foldl' go acc xs
  where
    go = Fold.foldl' f

-- -- ** Legacy Fold

fold :: Fold.Foldable c => (a -> b -> b) -> b -> SetsWith k c a -> b
fold f acc (SetsWith (_,xs)) = Map.fold go acc xs
  where
    go cs acc' = Fold.foldr f acc' cs

-- -- * Min/Max

findMin :: (Ord a, Fold.Foldable c) => SetsWith k c a -> a
findMin (SetsWith (_,xs)) = Fold.minimum $ snd $ Map.findMin xs

findMax :: (Ord a, Fold.Foldable c) => SetsWith k c a -> a
findMax (SetsWith (_,xs)) = Fold.maximum $ snd $ Map.findMax xs

-- | Deletes __entire set__ with minimum key
deleteMin :: SetsWith k c a -> SetsWith k c a
deleteMin (SetsWith (f,xs)) = SetsWith (f, Map.deleteMin xs)

deleteMax :: SetsWith k c a -> SetsWith k c a
deleteMax (SetsWith (f,xs)) = SetsWith (f, Map.deleteMax xs)

deleteFindMin :: SetsWith k c a -> (c a, SetsWith k c a)
deleteFindMin (SetsWith (f,xs)) = let ((_,l),zs) = Map.deleteFindMin xs
                                  in (l, SetsWith (f,zs))

deleteFindMax :: SetsWith k c a -> (c a, SetsWith k c a)
deleteFindMax (SetsWith (f,xs)) = let ((_,l),zs) = Map.deleteFindMax xs
                                  in (l, SetsWith (f,zs))

minView :: SetsWith k c a -> Maybe (c a, SetsWith k c a)
minView (SetsWith (f,xs)) = (\(l,a) -> (l, SetsWith (f,a))) <$> Map.minView xs

maxView :: SetsWith k c a -> Maybe (c a, SetsWith k c a)
maxView (SetsWith (f,xs)) = (\(l,a) -> (l, SetsWith (f,a))) <$> Map.maxView xs

-- -- * Conversion

elems :: ( Sets.HasUnion (c a)
         , Sets.HasEmpty (c a)
         ) => SetsWith k c a -> c a
elems (SetsWith (_,xs)) = Sets.unions $ Map.elems xs

toList :: SetsWith k c a -> (a -> k, [c a])
toList (SetsWith (f,xs)) = (f, Map.elems xs)

fromList :: ( Ord k
            , Sets.HasSingleton a (c a)
            , Sets.HasUnion (c a)
            ) => (a -> k) -> [a] -> SetsWith k c a
fromList f = Fold.foldr insert $ empty f

-- -- * Ordered List

toAscList :: SetsWith k c a -> [c a]
toAscList (SetsWith (_,xs)) = snd <$> Map.toAscList xs

toDescList :: SetsWith k c a -> [c a]
toDescList (SetsWith (_,xs)) = snd <$> Map.toDescList xs

fromAscList :: ( Eq k
               , Sets.HasSingleton a (c a)
               ) => (a -> k) -> [a] -> SetsWith k c a
fromAscList f xs = SetsWith (f, Map.fromAscList $ (f <$> xs) `zip` fmap Sets.singleton xs)

fromDistinctAscList :: Sets.HasSingleton a (c a) => (a -> k) -> [a] -> SetsWith k c a
fromDistinctAscList f xs = SetsWith (f, Map.fromDistinctAscList $ (f <$> xs) `zip` fmap Sets.singleton xs)

-- -- * Debugging

showTree :: (Show k, Show (c a)) => SetsWith k c a -> String
showTree (SetsWith (_,xs)) = Map.showTree xs

showTreeWith :: (k -> c a -> String) -> Bool -> Bool -> SetsWith k c a -> String
showTreeWith f a b (SetsWith (_,xs)) = Map.showTreeWith f a b xs
