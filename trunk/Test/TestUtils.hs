{-
  Tests the Data.GameTheory.Utils module
-}
module Test.TestUtils where

import Test.QuickCheck

import Data.GameTheory.Utils

newtype LargeRangeDouble = LRDouble Double
    deriving (Eq, Show)

-- skew the distribution over doubles so that we get some
-- interesting ones with large exponents, to exercise that
-- part of the code in readDouble
instance Arbitrary LargeRangeDouble where
    arbitrary = do x <- choose (0.0,1.0)
                   exp <- choose (-200.0,200.0)
                   return $ LRDouble $ x * (10.0 ** exp)
    
prop_read_double (LRDouble d) = let d' = readDouble $ show d
                                in if d == 0.0
                                   then d' == 0.0
                                   else abs((d - d') / d) < 1e-9

