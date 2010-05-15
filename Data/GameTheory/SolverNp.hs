{-
  Implements the n-player algorithm described in the paper, where it
  is defined as "Algorithm 2" in Section 6 of the journal version.
 -}
module Data.GameTheory.SolverNp (solveNp) where

import Control.Monad
import Control.Monad.Error
import Data.List ((\\), mapAccumL)
import LevMar.Intermediate
import Numeric.LinearProgramming

import Data.GameTheory.Nfg
import Data.GameTheory.Iterators
import Data.GameTheory.NashError
import Data.GameTheory.LpSolver

-- parameters for levmar
eps = 1e-6
max_iterations = 1000
      
opts :: Options Double
opts = defaultOpts { optStopNormInfJacTe = 1e-9
                   , optStopNorm2Dp      = 1e-9
                   , optStopNorm2E       = eps
                   }

{-
  In this module, we use both the Levenberg–Marquardt algorithm and
  linear programming.  The former is a nonlinear least squares solver,
  and can thus always be used (the nonlinearity arises from calculating
  the probability of an action profile, which may require taking the
  product of probabilities, one for each of the n players).  The latter
  is faster, but can only be applied in limited circumstances.

  In order to abstract common functionality, we build up functions using
  shared code, and then just write code to convert these functions into
  the types required by the individual solvers.

  * Var: tuple where the first element is a variable in the problem
         formulation (e.g., a player's action), and the second element
         is the index of the variable in that we will use in the solver.
  * Cell: tuple consisting of a coefficient and a list of solver variables
          that are multiplied together.  For example, if x_i is the i-th
          solver variable, then "(2.3, [4,5])" corresponds to the term:
              2.3 * x_4 * x_5
  * Function: a sum of cells
-}
type Var = (Int,Int)
type Cell = (Double,[Int])
type Func = [Cell]

-- A function of this type takes the next variable index to assign
-- and a list to assign variables to.  It returns the updated
-- next variable index, and pairs which specify the variable
-- for each value in the input list.
type Assigner = Int -> [Int] -> (Int, [Var])

assign :: Assigner
assign = mapAccumL (\v x -> (v + 1, (x,v)))
         
-- if a player has only one strategy in their support,
-- then we do not create a probability variable for it
assignXs :: Assigner
assignXs v [x] = (v, [(x,-1)])
assignXs v xs = assign v xs
                
toVars :: Assigner -> Int -> [[Int]] -> (Int, [[Var]])
toVars a v = mapAccumL a v

-- Extracts a solution from either levmar or LP.
-- The special handling for a support of size 1 is required, because
-- we do not solve for the strategy of a player whose support contains
-- only a single action.
extractSol :: SupportProfile -> [Double] -> StrategyProfile
extractSol [] _ = []
extractSol ([ai]:sis) ps = [(ai, 1.0)]:(extractSol sis ps)
extractSol (si:sis) ps =
    let (headPs, tailPs) = splitAt (length si) ps
        strat = zipWith (\x y -> (x,y)) si headPs
    in strat:(extractSol sis tailPs)
       
{-
  Creates functions for a given player's action.  If this action is in
  the player's support, then it must be equal to the player's utility
  in equilibrium.  Otherwise, it must not exceed it.
 -}
mkFuncs :: Game -> [[Var]] -> [Int] -> [Support] -> [[Func]]
mkFuncs g xs us ais = fsIter 0 ais us
    where fsIter :: Int -> [Support] -> [Int] -> [[Func]]
          fsIter p [] [] | p == length us = [[]]
          fsIter p (si:sis) (u:us) = (iter p si u):(fsIter (p+1) sis us)
          iter :: Int -> [Int] -> Int -> [Func]
          iter p [] u = []
          iter p (a:as) u  = eq:(iter p as u)
              where mkAll :: [[Var]] -> [[Var]]
                    mkAll [] = [[]]
                    mkAll (y:ys) = [(aj:ajs) | ajs <- mkAll ys, aj <- y]
                    xMinusI = take p xs ++ drop (p+1) xs
                    mkCell :: [Var] -> Cell
                    mkCell aMinusI = (pay aMinusI, map snd aMinusI)
                    pay aMinusI = payoff g p $ mkPayoffIdx g $
                                  inject p a (map fst aMinusI)
                    eq = (-1.0,[u]):(map mkCell (mkAll xMinusI))
                    inject p ai aMinusI = (take p aMinusI) ++ [ai] ++
                                          (drop p aMinusI)

-- returns, for each player, the actions *outside* of their support
mkNonAis g ais = zipWith ((\\)) allActions ais
    where allActions = map (\x -> [0..x-1]) (numActions g)

{-
For efficiency reasons, we want to limit the number of times we invoke
solvers.  Furthermore, when we do use a solver, we want to use an LP solver,
because it generally runs faster than a general-purpose, nonlinear solver.
Thus, we vary the solver based on the number of players who have more
than one action in their support.
  case 0: Because each player plays a pure strategy, there is nothing to solve.
  case 1: All equations in Feasibility Program 1 (see the paper) are
          linear.  Thus, we can simply invoke the LP solver.
  case 2: For the two players with supports of size greater than 1, the
          equations are linear.  However, for all remaining players, the
          equations are nonlinear, because they have to multiply a probability
          for both of these players in order to calculate the probability
          of a realizing a payoff.

          On our first pass, we simply solve for an equilibrium between
          the two players, and just check afterwards that any solution
          we found is indeed a Nash equilibrium.  Because this is an
          incomplete method, on our second pass we retry this case
          using the full nonlinear solver.
  case >2: Use the full nonlinear, levmar solver.
-}
solveNp :: Game -> ThrowsError StrategyProfile
solveNp g = let sup = supportsNp g
                noSol = throwError $ SolverError "found no solution"
            in iter sup solve1 $
               iter sup solve2 noSol
    where iter [] _ onEmpty = onEmpty
          iter (x:xs) solve onEmpty
              = do sol <- solve x
                   case sol of
                     Just s -> return s
                     Nothing -> iter xs solve onEmpty
          solve1 :: SupportProfile -> ThrowsError (Maybe StrategyProfile)
          solve1 ais = case numNonPure ais of
                         0 -> return $ Just $ map (\[ai] -> [(ai,1.0)]) ais
                         1 -> solveUsingLp normalizedG True  ais
                         2 -> solveUsingLp normalizedG False ais
                         _ -> solveUsingLevMar normalizedG ais
          solve2 ais = case numNonPure ais of
                         2 -> solveUsingLevMar normalizedG ais
                         _ -> return Nothing
          numNonPure = length . (filter ((>1) . length))
          normalizedG = normalize g

-- Invokes the same LP solver as the 2-player algorithm, using a similar
-- problem formulation.
solveUsingLp :: Game -> Bool -> [Support] -> ThrowsError (Maybe StrategyProfile)
solveUsingLp g full ais
    = let xs :: [[Var]]
          (nextVar, xs) = toVars assignXs 1 ais

          n = length $ players g
          us = [nextVar .. (nextVar + n - 1)]
          nonAis = mkNonAis g ais

          -- Constraints:
          --   (1) actions in supports
          --   (2) actions not in supports
          --   (3) probabilities sum to 1
          allCons = Sparse $ inSupCons ++ outSupCons ++ probCons
              where inSupCons = map (:==: 0) $ mkSupCons $ forCons ais
                    outSupCons = map (:&: (lb_payoff - ub_payoff, 0.0)) $
                                 mkSupCons $ forCons nonAis
                    probCons = map mkProbCons $ filter ((>1) . length) xs
                    -- helpers for making constraints
                    mkProbCons = (:==: 1) . map ((1#) . snd)
                    mkSupCons = map toCons . concat . mkFuncs g xs us
                    toCons [] = []
                    toCons ((c,vs):ys) = (c#var):(toCons ys)
                        where var = case filter (>=0) vs of
                                      [v] -> v
                                      vars -> error $ "bad vars: " ++ show vs
                    forCons ys = if full then ais else iter ys (map length ais)
                        where iter [] [] = []
                              iter (y:ys) (1:lens) = []:(iter ys lens)
                              iter (y:ys) (_:lens) =  y:(iter ys lens)
                   
          -- Bounds: probabilities must be in [0,1]
          -- and the values are free
          bounds :: Bounds
          bounds = iter (concat xs) (map Free us)
              where iter [] res = res
                    iter ((x,v):xs) res | v == -1   = iter xs res
                                        | otherwise = iter xs ((toBound v):res)
                    toBound v = v:&:(0.0, 1.0)
          res = solveLp allCons bounds (extractSol ais)
      in if full then res else res >>= \ms -> return (ms >>= checkSol)
          where checkSol :: StrategyProfile -> Maybe StrategyProfile
                checkSol sol = case checkNashEq g sol of
                                 Just _  -> Nothing
                                 Nothing -> Just sol

{- Formulates the feasibility problem as a Least Squares problem, and then
   invokes the solver of the levmar library.  Feasibility Program 1 is
   converted to a Least Squares problem by:
     (1) converting the equations for actions in a player's support into
         a difference between the left- and right-hand sides.  In any
         solution, this difference will be zero.
     (2) Doing the same for actions *outside* of a player's, but also
         introducing a slack variable, which represents the amount that the
         player would gain by switching to any action in their support.
         In any solution, this gain must not be negative, and thus the
         slack variable is constrained to be positive.
-}
solveUsingLevMar :: Game -> [Support] -> ThrowsError (Maybe StrategyProfile)
solveUsingLevMar g ais
    = let xs :: [[Var]]
          (numXs, xs) = toVars assignXs 0 ais

          n = length $ players g
          us = [numXs .. (numXs + n - 1)]
          nonAis = mkNonAis g ais
                   
          (numVars, slacks) = toVars assign (numXs + n) nonAis
          numSlacks = sum $ map length slacks
                  
          -- equality constraints, for variables in the support
          -- slack constraints, for variables outside the support
          fs = eqFs ++ slackFs
              where toFuncs = mkFuncs g xs us
                    eqFs = concat $ toFuncs ais
                    slackFs = concat $ zipWith (zipWith addSlack)
                              slacks (toFuncs nonAis)
                    addSlack (s,v) = ((1.0,[v]):)
                
          -- initial params
          params = uniformProbs ais ++
                   replicate n 0.0 ++
                   replicate numSlacks 0.0 
              where uniformProbs = concat . map (toUniform . length)
                    toUniform 1 = []
                    toUniform x = replicate x (1.0 / (fromIntegral x))

          -- samples: both equality and slack constraints should sum to 0
          samples = replicate (length fs) 0.0
                          
          -- bounds
          lbs = replicate numXs 0.0 ++
                replicate n lb_payoff ++
                replicate numSlacks 0.0
          ubs = replicate numXs 1.0 ++
                replicate n ub_payoff ++
                replicate numSlacks ub_payoff

          -- linear constraints: probs sum to 1
          mkCons x = let s = snd $ x !! 0
                         c = length x
                     in replicate s 0.0 ++
                        replicate c 1.0 ++
                        replicate (numVars - c - s) 0.0
          cons = let withVars = filter ((>1) . length) xs
                 in (map mkCons withVars, replicate (length withVars) 1.0)
                    
          res = run_levmar (mkModel fs) (mkJac fs) params samples lbs ubs cons
          toDebug reason = debug reason res
                           xs us slacks fs params samples lbs ubs cons
      in case res of
           Left err -> throwError $ SolverError $ show err
           Right (ps, inf, covar) -> 
               let found = infNorm2E inf < eps
               in case infStopReason inf of
                    SmallNorm2E ->
                        if found -- then first check for ourselves
                        then let s = extractSol ais (take numXs ps)
                             in case checkNashEq g s of
                                  Nothing -> return $ Just s
                                  Just dev -> throwError $ SolverError $
                                              "Deviation " ++ show dev
                        else toDebug "SmallNorm2E, but !found"
                    SmallDp -> if not found
                               then return Nothing
                               else toDebug "SmallDp, but found"
                    -- TODO: On some games, the solver fails to find a
                    -- Nash equilibrium due to a SmallGradient.  I do not
                    -- understand why this occurs, and I cannot simply
                    -- return Nothing here without making the solver incomplete.
                    SmallGradient -> toDebug "Unexpected small gradient"
                    -- Of course, MaxIterations also needs to be handled,
                    -- but I haven't seen this occur after I added
                    -- support for using a Jacobian.
                    MaxIterations -> return Nothing
                    -- And I've never seen any other type of return value.
                    -- Of course, these would then need to be handled.
                    _ -> toDebug "Unhandled case"

-- handles the fake variables we create for pure strategies
getVal vs x | x == -1   = 1
            | otherwise = vs !! x

-- converts a list of functions into a model for levmar
mkModel :: [Func] -> Model Double
mkModel fs = \vs -> map (evalFunc vs) fs
    where evalFunc :: [Double] -> Func -> Double
          evalFunc vs f = sum $ map (evalCell vs) f
          evalCell :: [Double] -> Cell -> Double
          evalCell vs (coeff,xs) = coeff * (product (map (getVal vs) xs))

-- converts a list of functions into a Jacobian matrix for levmar
mkJac :: [Func] -> Jacobian Double
mkJac fs = \vs -> map (evalFunc vs) fs
    where evalFunc :: [Double] -> Func -> [Double]
          evalFunc vs f = map (evalFuncAt vs f) [0..length vs - 1]
          evalFuncAt :: [Double] -> Func -> Int -> Double
          evalFuncAt vs f i = sum $ map (evalCellAt i vs) f
          evalCellAt :: Int -> [Double] -> Cell -> Double
          evalCellAt i vs (coeff,xs)
                     | i `elem` xs = let vals = map (getVal vs) $ xs \\ [i]
                                     in coeff * (product vals)
                     | otherwise   = 0.0

-- invokes the library's solver, combining the input generated by
-- solveUsingLevMar with the configuration parameters defined
-- at the top of this file
run_levmar model jac params samples lbs ubs cons
    = levmar
      model
      (Just jac)
      params
      samples
      max_iterations
      opts
      (Just lbs)
      (Just ubs)
      (Just cons)
      Nothing

-- helper type for the return value of the levmar solver
type Result n = Either LevMarError
                       ( [Double]
                       , Info Double
                       , CovarMatrix Double
                       )
      
-- print everything out, to debug solver
debug reason res xs us slacks fs params samples lbs ubs cons = 
    throwError $ SolverError $ "\n" ++ reason ++ 
                   "\n  VARS: " ++ varIter 0 xs us slacks ++ "\n\n" ++
                   "  FUNCTIONS: " ++ fsIter fs samples ++ "\n\n" ++
                   "  PARAMS: " ++ paramIter params lbs ubs ++ "\n\n" ++
                   "  CONS: " ++ consIter (fst cons) (snd cons) ++ "\n\n" ++
                   "  RESULT:\n" ++ printInteresting res
    where varIter _ [] [] [] = ""
          varIter p (x:xs) (u:us) (s:ss) =
              "\n    Player " ++ show p ++ ": " ++ show x ++
              ", " ++ show u ++ ", " ++ show s ++
              varIter (p+1) xs us ss
          fsIter [] [] = ""
          fsIter (f:fs) (s:ss) =
              "\n    " ++ show f ++ " @ " ++ show s ++
              fsIter fs ss
          paramIter [] [] [] = ""
          paramIter (p:ps) (x:xs) (y:ys) =
              "\n    " ++ show p ++ " in " ++ show (x,y) ++
              paramIter ps xs ys
          consIter [] [] = ""
          consIter (a:as) (b:bs) =
              "\n    " ++ show a ++ " = " ++ show b ++
              consIter as bs
          printInteresting :: Result n -> String
          printInteresting (Left err) = "Error: " ++ show err
          printInteresting (Right (ps, inf, covar)) 
              = "infStopReason = " ++ show (infStopReason inf) ++ "\n" ++
                "infNorm2E     = " ++ show (infNorm2E     inf) ++ "\n" ++
                "infNumIter    = " ++ show (infNumIter    inf) ++ "\n" ++
                "ps            = " ++ show ps
