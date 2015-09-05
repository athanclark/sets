module Data.Set.Unordered.Many.Data where

import qualified Data.Set.Unordered.Many as UM
import Data.Set.Class



umsetTo :: Int -> UM.UMSet Int
umsetTo n = fromFoldable [1..n]

umset1 :: UM.UMSet Int
umset1 = umsetTo 10

umset2 :: UM.UMSet Int
umset2 = umsetTo 20

umset3 :: UM.UMSet Int
umset3 = umsetTo 30

umset4 :: UM.UMSet Int
umset4 = umsetTo 40

umset5 :: UM.UMSet Int
umset5 = umsetTo 50
