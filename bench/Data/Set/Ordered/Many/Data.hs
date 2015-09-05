module Data.Set.Ordered.Many.Data where

import qualified Data.Set.Ordered.Many as OM
import Data.Set.Class



omsetTo :: Int -> OM.OMSet Int
omsetTo n = fromFoldable [1..n]

omset1 :: OM.OMSet Int
omset1 = omsetTo 10

omset2 :: OM.OMSet Int
omset2 = omsetTo 20

omset3 :: OM.OMSet Int
omset3 = omsetTo 30

omset4 :: OM.OMSet Int
omset4 = omsetTo 40

omset5 :: OM.OMSet Int
omset5 = omsetTo 50
