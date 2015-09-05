module Data.Set.Unordered.Unique.Data where

import qualified Data.Set.Unordered.Unique as UU
import Data.Set.Class



uusetTo :: Int -> UU.UUSet Int
uusetTo n = fromFoldable [1..n]

uuset1 :: UU.UUSet Int
uuset1 = uusetTo 10

uuset2 :: UU.UUSet Int
uuset2 = uusetTo 20

uuset3 :: UU.UUSet Int
uuset3 = uusetTo 30

uuset4 :: UU.UUSet Int
uuset4 = uusetTo 40

uuset5 :: UU.UUSet Int
uuset5 = uusetTo 50
