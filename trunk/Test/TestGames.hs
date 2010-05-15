{-
  Contains data structures and functions useful for creating
  games for QuickCheck tests.
-}
module Test.TestGames where

import Test.QuickCheck
import Control.Monad
import Data.Array
import Data.List ((\\), subsequences)
    
import Data.GameTheory.Nfg

-- creates a game, given the number of actions for all players,
-- and the payoffs for all players
mkTestGame ns payoffs = mkGame "" (replicate (length ns) "") ns $ payoffs

newtype Actions = Actions Int
    deriving (Eq, Show)

instance Arbitrary Actions where
    arbitrary = liftM Actions $ choose (1,10)

newtype NPlayerActions = NPlayerActions [Int]
    deriving (Eq, Show)

instance Arbitrary NPlayerActions where
    arbitrary = do p <- choose (3,4)
                   liftM NPlayerActions $ sequence $
                         replicate p $ choose (1,4)
             
-- simplest possible game, which allows us to test iteration over
-- support profiles without worrying about dominated strategies
newtype ConstantPayoffGame = ConstantPayoffGame Game
    deriving (Eq, Show)
       
newtype NpConstantPayoffGame = NpConstantPayoffGame Game
    deriving (Eq, Show)

data DomActionGame = DomActionGame [Int] Game
    deriving (Eq, Show)

data NpDomActionGame = NpDomActionGame [Int] Game
    deriving (Eq, Show)

-- Creates a game a single action is dominated for each player.
-- The first argument is a constructor, and the second is the
-- the number of players.
mkDom :: ([Int] -> Game -> a) -> Int -> Gen a
mkDom cons ps = do ns <- sequence $ replicate ps $ choose (2,4)
                   ds <- mapM (\n -> choose (0,n-1)) ns
                   return $ cons ds $ mkTestGame ns $
                          map (mkPayoffs ns) (zip [0..ps-1] ds)
    where mkPayoffs :: [Int] -> (Int,Int) -> [Double]
          mkPayoffs ns (p,d) = map (pay p d) (mkAll ns)
          pay :: Int -> Int -> [Int] -> Double
          pay p d a = if (a !! p) == d then 0 else 1
          mkAll :: [Int] -> [[Int]]
          mkAll [] = [[]]
          mkAll (n:ns) = [(a:as) | as <- mkAll ns, a <- [0..n-1]]

instance Arbitrary DomActionGame where
    arbitrary = mkDom DomActionGame 2

instance Arbitrary NpDomActionGame where
    arbitrary = choose (2,4) >>= mkDom NpDomActionGame

newtype RandomGame = RandomGame Game
    deriving (Eq, Show)

newtype NpRandomGame = NpRandomGame Game
    deriving (Eq, Show)

-- Creates a game in which every payoff is independently selected
-- from the uniform distribution over [a,b], where a and b are the
-- second and third arguments.  The last argument is the number of players.
mkRandomGame :: (Game -> a) -> Double -> Double -> Int -> Gen a
mkRandomGame cons a b ps =
    do ns <- sequence $ replicate ps $ choose (3,4)
       payoffs <- sequence $ replicate ps $
                  sequence $ replicate (product ns) (choose (a,b))
       return $ cons $ mkTestGame ns payoffs
                                             
instance Arbitrary RandomGame where
    arbitrary = mkRandomGame RandomGame 0.0 1.0 2

instance Arbitrary NpRandomGame where
    arbitrary = choose (2,3) >>=
                mkRandomGame NpRandomGame 0.0 1.0

-- By specifies bounds on the payoffs that are equal, we create
-- constant-payoff games.
instance Arbitrary ConstantPayoffGame where
    arbitrary = mkRandomGame ConstantPayoffGame 1.0 1.0 2

instance Arbitrary NpConstantPayoffGame where
    arbitrary = choose (2,3) >>=
                mkRandomGame NpConstantPayoffGame 1.0 1.0
