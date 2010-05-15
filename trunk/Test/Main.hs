{-
  Uses QuickCheck to tests both individual components, and the
  full solvers on randomly created games.
-}
import Test.QuickCheck
import Text.Printf
import Control.Monad
import Data.Array
import Data.List ((\\), subsequences)
    
import Data.GameTheory.Iterators
import Data.GameTheory.Solver2p
import Data.GameTheory.SolverNp
import Data.GameTheory.Nfg

import Test.TestUtils
import Test.TestGames

-- Verifies that the two lists are permutations of each other.
permutation :: (Eq a) => [a] -> [a] -> Bool
permutation xs ys = xs \\ ys == [] && ys \\ xs == []

prop_2p_ssp_domain (Actions a) (Actions b) =
    let all = [(x,y) | x <- [1..a], y <- [1..b]]
    in permutation (supportSizeProfiles2p a b) all

prop_np_ssp_domain (NPlayerActions ns) =
    permutation (supportSizeProfilesNp ns) (iter ns)
    where iter :: [Int] -> [[Int]]
          iter [] = [[]]
          iter (n:ns) = concat $ map (expand n) (iter ns)
          expand :: Int -> [Int] -> [[Int]]
          expand n ssp = map (:ssp) [1..n]

-- Verifies that support profiles for the 2-player algorithm
-- are ordered as specified in the paper.
ordered2p :: [(Int,Int)] -> Bool
ordered2p [] = True
ordered2p [_] = True
ordered2p ((a,b):y@(a',b'):ys) = ordered2p (y:ys) &&
                               let d = abs (a - b)
                                   d' = abs (a' - b')
                               in (d < d') || (d == d' && a + b <= a' + b')

-- For the n-player algorithm, there is a different ordering
-- that we need to verify.
orderedNp :: [[Int]] -> Bool
orderedNp [] = True
orderedNp [_] = True
orderedNp (ns:ns':nss) = orderedNp (ns':nss) &&
                         let s = sum ns
                             s' = sum ns'
                         in (s < s') || (s == s' && maxdiff ns <= maxdiff ns')
    where maxdiff :: [Int] -> Int
          maxdiff xs = maximum [ x - y | x <- xs, y <- xs]

prop_2p_ssp_ordered (Actions a) (Actions b)
    = ordered2p $ supportSizeProfiles2p a b

prop_2p_support_ordered (ConstantPayoffGame g)
    = ordered2p $ map toLen $ supports2p g
      where toLen (s1,s2) = (length s1, length s2)

prop_np_ssp_ordered (NPlayerActions as)
    = orderedNp $ supportSizeProfilesNp as

prop_np_support_ordered (NpConstantPayoffGame g)
    = orderedNp $ map (map length) (supportsNp g)

-- the set of possible supports is all of the subsets of
-- the actions, except for the empty list, which is the
-- first element of 'subsequences'
allSupports :: Int -> [[Int]]
allSupports n = tail $ subsequences [0..(n-1)]

prop_2p_support_domain (ConstantPayoffGame g@(Game _ _ [n1,n2] _))
    = let all = [(x,y) | x <- allSupports n1, y <- allSupports n2]
      in permutation (supports2p g) all

prop_np_support_domain (NpConstantPayoffGame g)
    = permutation (supportsNp g) (mkAll (numActions g))
      where mkAll [] = [[]]
            mkAll (a:as) = [(x:xs) | x <- allSupports a, xs <- mkAll as]

-- Creates all support profiles that do not include the single, dominated
-- action for each player specified in the first argument.
allOneDom ds g = iter ds (numActions g)
    where iter [] [] = [[]]
          iter (d:ds) (a:as) = [(x:xs) | x <- valid d a, xs <- iter ds as]
          valid d a = filter (not . (elem d)) (allSupports a)
         
prop_2p_support_one_dom (DomActionGame ds g)
    = let s = (allOneDom ds g)
      in permutation (supports2p g) (map (\x -> (x !! 0, x !! 1)) s)

prop_np_support_one_dom (NpDomActionGame ds g)
    = permutation (supportsNp g) (allOneDom ds g)

-- Verifies that the input solver produces a valid Nash equilibrium
-- on the input game.
checkNE solve g = case solve g >>= return . (checkNashEq g) of 
                    Left err         -> False
                    Right (Just dev) -> False
                    Right Nothing    -> True

prop_2p_nash_eq (RandomGame g) = checkNE solve2p g
prop_np_nash_eq (NpRandomGame g) = checkNE solveNp g

-- This checks that our verification of Nash equilibria isn't just
-- letting everything through.  We does this by "verifying" a strategy profile
-- in which each player randomizes over all of their actions.  On a
-- random game, this is a Nash equilibrium with probability zero
-- (with massive hand-waving, and ignoring epsilons).
prop_2p_verify_non_ne (RandomGame g) =
    let mkUniform :: Int -> [(Int,Double)]
        mkUniform n = map (\a -> (a, 1.0 / (fromIntegral n))) [0..n-1]
        uniform = map mkUniform $ numActions g
    in case checkNashEq g uniform of
         Just _ -> True
         Nothing -> False

tests = [("Utils.readDouble", quickCheck prop_read_double)
        ,("two-player nash eq on random games", quickCheck prop_2p_nash_eq)
        ,("n-player nash eq on random games", quickCheck prop_np_nash_eq)
        ,("two-player support size domain", quickCheck prop_2p_ssp_domain)
        ,("two-player support size ordered", quickCheck prop_2p_ssp_ordered)
        ,("two-player support domain", quickCheck prop_2p_support_domain)
        ,("two-player support ordered", quickCheck prop_2p_support_ordered)
        ,("two-player support domain, one dominated",
          quickCheck prop_2p_support_one_dom)
        ,("two-player verification catches non-equilibria",
          quickCheck prop_2p_verify_non_ne)
        ,("n-player support size domain", quickCheck prop_np_ssp_domain)
        ,("n-player support size ordered", quickCheck prop_np_ssp_ordered)
        ,("n-player support domain", quickCheck prop_np_support_domain)
        ,("n-player support ordered", quickCheck prop_np_support_ordered)
        ,("n-player support domain, one dominated",
          quickCheck prop_np_support_one_dom)
        ]

main  = mapM_ (\(s,a) -> printf "%-25s: " s >> a) tests
