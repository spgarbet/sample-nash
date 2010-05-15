{-
  Implements the 2-player algorithm described in the paper, where it
  is defined as "Algorithm 1" in Section 5 of the journal version.
 -}
module Data.GameTheory.Solver2p (solve2p) where

import Control.Monad
import Control.Monad.Error
import Data.List ((\\))
import Numeric.LinearProgramming
        
import Data.GameTheory.Nfg
import Data.GameTheory.Iterators (supports2p)
import Data.GameTheory.LpSolver
import Data.GameTheory.NashError

solve2p :: Game -> ThrowsError StrategyProfile
solve2p g | length (players g) /= 2 = throwError $ SolverError "wrong solver"
          | otherwise               = iter (supports2p g)
    where iter :: [(Support,Support)] -> ThrowsError StrategyProfile
          iter []     = throwError $ SolverError "found no solution"
          iter (x:xs) = do sol <- solve x
                           case sol of
                             Just s -> return s
                             Nothing -> iter xs
          -- constructs a strategy profile, where the first arg is the player,
          -- 'a' is the action for that player, and 'b' is the action for
          -- the other player.
          prof 0 a b = [a,b]
          prof 1 a b = [b,a]
          normalizedG = normalize g
          coeffs p bs a = (-1):(map pay bs)
              where pay b = payoff normalizedG p $
                            mkPayoffIdx normalizedG $
                            prof p a b
          mkCons p bs vs a = (zipWith (#) (coeffs p bs a) vs)

          solve :: (Support,Support) -> ThrowsError (Maybe StrategyProfile)
          solve ([a1],[a2]) =
              -- the input is a pure strategy profile, and we know that
              -- the iterator never gives us actions that are conditionally
              -- dominated.  Thus, we can simply return a Nash equilibrium
              return $ Just [[(a1,1.0)], [(a2,1.0)]]
          -- Implements "Feasibility Program 1" in the paper.
          -- See Section 4 for more details.
          solve (s1,s2) =
              let c1 = length s1
                  c2 = length s2
                                   
                  -- Variables in the LP
                  ps1 = [   1 ..    c1] -- player 1 probabilities
                  ps2 = [c1+1 .. c1+c2] -- player 2 probabilities
                  v1  = c1 + c2 + 1     -- player 1 value
                  v2  = v1 + 1          -- player 2 value
                  
                  -- lists of pure strategies: we already have the input
                  -- supports of s1 and s2, and we need their complements below
                  [n1,n2] = (numActions g)
                  s1C = [0 .. n1-1] \\ s1 -- complement of s1
                  s2C = [0 .. n2-1] \\ s2 -- complement of s2
                  
                  -- Support constraints: For both players, there is a similar
                  -- to constraint for actions in the support and actions
                  -- outside the support. The difference is that the former
                  -- must yield the value for the player, while the latter
                  -- must not provide a greater value.
                  mkCons1 = map (mkCons 0 s2 (v1:ps2))
                  mkCons2 = map (mkCons 1 s1 (v2:ps1))
                  inSupportCons = map (:==: 0) $
                                  mkCons1 s1 ++ mkCons2 s2
                  outSupportCons = map (:&: (lb_payoff - ub_payoff,0.0)) $
                                   mkCons1 s1C ++ mkCons2 s2C

                  -- Probability constraints: Here we just force the
                  --- probabilities for both players to sum to one.
                  -- Range is enforced in the bounds.
                  mkProbCons = (:==: 1) . map (1#)
                  probCons = [mkProbCons ps1, mkProbCons ps2]

                  -- aggregate all constraints
                  allCons = Sparse $ inSupportCons ++ outSupportCons ++ probCons

                  -- Bounds: probabilities must be in [0,1]
                  -- and the values are free
                  bounds = [Free v1, Free v2] ++
                           map (:&:(0.0, 1.0)) [1 .. last ps2]
                        
                  extractSol res = [ zip s1 res
                                   , zip s2 (drop c1 res)]       
              in solveLp allCons bounds extractSol
