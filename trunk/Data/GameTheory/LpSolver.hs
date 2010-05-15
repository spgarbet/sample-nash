{-
  Contains code for solving an LP using the Numeric.LinearProgramming library,
  which is invoked by both solvers.  Handles the various possible return types.
-}
module Data.GameTheory.LpSolver (solveLp) where

import Control.Monad
import Control.Monad.Error
import Numeric.LinearProgramming
        
import Data.GameTheory.Nfg
import Data.GameTheory.NashError

solveLp :: Constraints -> Bounds -> ([Double] -> StrategyProfile) -> ThrowsError (Maybe StrategyProfile)
solveLp cons bounds extractSol =
    let -- Objective function: this is a dummy function,
        -- because we have nothing to optimize-- this is a feasibility problem
        obj = Maximize $ replicate (length bounds) 0
    in case simplex obj cons bounds of
         Optimal (_,res) -> return $ Just $ extractSol res
         Infeasible _    -> return Nothing
         NoFeasible      -> return Nothing
         -- We've never encountered any of the other return values, so we
         -- simply bail.  We're uncomfortable returning "no solution" here,
         -- because we don't know whether such a case indicates a bug,
         -- such as a mistake in formulating the LP.
         s -> throwError $ SolverError $ "Unhandled " ++
              "Numeric.LinearProgramming.Solution: " ++ show s
