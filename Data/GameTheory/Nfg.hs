{-
  Contains the core data structures related to normal form games,
  along with some helper functions.
-}
module Data.GameTheory.Nfg where

import Data.Array
import qualified Data.DList as D
import Data.List (foldl')
import Control.Monad.Error
    
import Data.GameTheory.NashError

{- We store the payoffs in an array of n arrays, where n is the number
 of players.  Within each player's array (of type PlayerPayoffs), the
 values are stored in the same order as they appear in the .nfg file.
 That is, the first payoff corresponds to the cells in which 
 each player plays their first action.  The next n values
 are for the same strategy profile, except that player 1's action
 is incremented, and so on.

 In order to efficiently access the inner array (which is implicitly
 a multi-dimensional array), we introduce a PayoffIdx
 type (which is simply an integer) and a multiplier for each player.
 Given an action profile, the index is its cross product with the list
 of multipliers.  For example, consider a two person game in which the
 first player has 5 actions, and the second has 3 actions.  The multipliers
 are then 1 and 5 respectively.  Thus, given the action profile (4,2), which
 means that player 1 players his 5th actions and player 2 plays his 3rd action,
 the PayoffIdx is 4*1 + 2*5 = 14.
 -}
type PlayerPayoffs = Array Int Double
    
data PayoffMatrix = PayoffMatrix { mults :: [Int]
                                 , payoffs :: Array Int PlayerPayoffs }
                    deriving (Eq, Show)

newtype PayoffIdx = PayoffIdx Int
    deriving (Show, Eq)

mkPayoffIdx :: Game -> ActionProfile -> PayoffIdx
mkPayoffIdx g as = PayoffIdx $ sum $ zipWith (*) (mults (matrix g)) as

payoff :: Game -> PlayerIdx -> PayoffIdx -> Double
payoff g p (PayoffIdx idx) = ((payoffs (matrix g)) ! p) ! idx
             
data Game = Game { title :: String,
                   players :: [String],
                   numActions :: [Int],
                   matrix :: PayoffMatrix }
          deriving (Eq)

-- Helper to create a Game.  It handles the conversion of a list of payoffs
-- into an instance of a PayoffMatrix, including calculating the
-- multipliers for each player.
mkGame :: String -> [String] -> [Int] -> [[Double]] -> Game
mkGame t ps ns pay = Game t ps ns $ PayoffMatrix (mults 1 ns) payoffs
    where payoffs = listArray (0,length ns - 1) $
                    map (listArray (0, product ns - 1)) pay
          mults _ [] = []
          mults m (n:ns) = m:(mults (m*n) ns)

-- exports a game in the .nfg format
toDotNfg :: Game -> String
toDotNfg g = D.toList $ D.concat $
             [ D.fromList "NFG 1 D "
             , addQuotes (title g)
             , D.fromList " { "
             , spaceSepList addQuotes (players g)
             , D.fromList "} { "
             , spaceSepList (D.fromList . show) (numActions g)
             , D.fromList "} \n\n"
             , spaceSepList (D.fromList . show) (flatten 0 0)
             ]
    where spaceSepList :: (a -> D.DList Char) -> [a] -> D.DList Char
          spaceSepList f = foldl' (\x -> D.append x . withSpace f) D.empty
          flatten :: Int -> Int -> [Double]
          flatten i j | j == product (numActions g) = []
                      | i == length (players g)     = flatten 0 (j + 1)
                      | otherwise = let p = ((payoffs (matrix g)) ! i) ! j
                                    in p:(flatten (i+1) j)
          withSpace f s = D.append (f s) $ D.fromList " "
          addQuotes s = D.cons '"' $ D.append (D.fromList s) $ D.fromList "\""
                                  
instance Show Game where
    show = toDotNfg

lb_payoff = 0.0
ub_payoff = 1.0
                              
-- Normalizes all payoffs to be in the range [0.0,1.0],
-- to allow us to use more precise bounds in the solver.
-- This is done on a per-player basis, since there is never
-- any need to compare payoffs between players.
normalize :: Game -> Game
normalize g = let (PayoffMatrix mults pay) = matrix g
                  pay' = fmap perPlayer $ pay
              in g { matrix = PayoffMatrix mults pay' }
    where perPlayer :: PlayerPayoffs -> PlayerPayoffs
          perPlayer ps = fmap perCell ps
              where lb = minimum $ elems ps
                    ub = maximum $ elems ps
                    perCell :: Double -> Double
                    perCell x | lb == ub  = 1.0
                              | otherwise = (x - lb) / (ub - lb)

-- A collection of aliases that add no type safety, but do
-- allow type signatures to be more self-documenting.
type PlayerIdx = Int
type ActionIdx = Int
type ActionProfile = [ActionIdx]
    
type Support = [ActionIdx]
type SupportProfile = [Support]

type SupportSize = Int
type SupportSizeProfile = [SupportSize]
    
type Probability = Double
type Strategy = [(ActionIdx,Probability)]
type StrategyProfile = [Strategy]

-- Records data about a profitable deviation for a player in a
-- putative Nash equilibrium.  The values are:
-- (player, action, value achieved in equilibrium, value achieved by deviating)
newtype Deviation = Deviation (Int,Int,Double,Double)
    deriving (Eq, Show)

-- Checks whether the input strategy profile is a Nash equilibrium.
-- If so, then it return Nothing.  Otherwise, it returns a profitable
-- deviation (out of possibly many).
checkNashEq :: Game -> StrategyProfile -> Maybe Deviation
checkNashEq g s = msum $ map byP [0 .. length (players g) - 1]
    where byP :: Int -> Maybe Deviation
          byP p = let v = value g p s
                     in msum $ map (tryPure p v) [0 .. (numActions g) !! p - 1]
          tryPure p v a = let alt = actionValue p a
                          in if alt < v + 1e-3
                             then Nothing
                             else Just $ Deviation (p,a,v,alt)
          actionValue p a = value g p $
                            -- replace p's strategy profile with a pure strategy
                            (take p s) ++ [[(a,1.0)]] ++ (drop (p+1) s)
          value g p s = iter s [] 1
              where iter [] as prob = (payoff g p (mkPayoffIdx g as)) * prob
                    iter (x:xs) as prob = sum $ (flip map) x $ \(a,pr) ->
                                          iter xs (as ++ [a]) (prob * pr)
