{-
  Produces lists of support-size profiles and support profiles
  that are used by the solvers.  Handles iterated removal of
  dominated strategies, as described in the paper.
-}
{-# LANGUAGE BangPatterns #-}
module Data.GameTheory.Iterators (supportSizeProfiles2p
                                 ,supports2p
                                 ,supportSizeProfilesNp
                                 ,supportsNp) where

import Data.Choose
import qualified Data.Array as A
    
import Data.GameTheory.Nfg

{-
  A component of the algorithms below is to iterate over profiles of
  strategies for n-1 players (that is, all players except for the
  player p that we are currently considering).  For each of these profiles,
  we want to compare the payoffs to player p for two individual actions.
  
  The simplest way to do this would be to combine each action with
  the profile for the other players to create a full strategy profile,
  and then to convert this profile into a PayoffIdx.  However, profiling
  showed this to be a bottleneck.

  Thus, we use the following hack for efficiency: We first inject into the
  profile of supports of other players a support for player p that only
  contains the action 0.  We then use the following mkIndices function
  to create a list PayoffIdxs corresponding to all possible strategy profiles.
  By construction, each of these indices will use action 0 player p.
  Then, when we want to compare the payoffs between two of player p's actions,
  we just need to the index the action times the player's multiplier
  (for a definition of the multiplier, see the comments above PayoffMatrix).

  This is the source of the mysterious, scattered instances of "[0]"
  in the code in this module.
-}

mkIndices :: Game -> SupportProfile -> [PayoffIdx]
mkIndices g sis = iter terms [PayoffIdx 0]
    where terms = zipWith (\m -> map (*m)) (mults (matrix g)) sis
          iter [] res = res
          iter (t:ts) res
              = let addIdx a = map (\(PayoffIdx i) -> PayoffIdx (i+a)) res
                    res' = concat $ map addIdx t
                in iter ts res'

{-
   Variables:
     * g = the game
     * p = the player for whom we are checking for a dominated strategy
     * indices = the PayoffIdxs corresponding the list of strategy profiles
                 described in the comments above.
     * a = the action of player p that we are checking.

   This function returns a boolean for whether 'a' is strictly dominated.
 -}
dom :: Game -> PlayerIdx -> [PayoffIdx] -> ActionIdx -> Bool
dom g p indices a = let maxA = ((numActions g) !! p) - 1
                    in iter maxA
    where !multForI = (mults (matrix g)) !! p
          !payoffsForI = (payoffs (matrix g)) A.! p
          iter (-1) = False
          iter a'   = if a /= a' && domByIter indices
                      then True
                      else iter (a' - 1)
              where domByIter [] = True
                    domByIter ((PayoffIdx i):is)
                        = if getPayoff a < getPayoff a'
                          then domByIter is
                          else False
                        where getPayoff a = payoffsForI A.! (i + a * multForI)

-- helper mehtod that checks whether any of the actions in the
-- support (the fourth argument) are dominated
existsDom :: Game -> PlayerIdx -> [PayoffIdx] -> Support -> Bool
existsDom g p indices si = iter si
    where iter [] = False
          iter (a:as) = if dom g p indices a then True else iter as
                                             
{-
  Returns the ordered list of supports for which the feasibility program
  is run for the 2-player algorithm (see "Algorithm 1" in the paper
  for more details).
 -}
supports2p :: Game -> [(Support,Support)]
supports2p g@(Game _ _ [n1,n2] _)
    = concat $ map ofSize (supportSizeProfiles2p n1 n2)
    where ofSize (x1,x2) = iter1 (Just (choose n1 x1)) x2
          iter1 Nothing _ = []
          iter1 (Just c) x2 =
              let s1 = elems c
                  s1Indices = mkIndices g [s1,[0]]
                  a2'Indices = mkIndices g [[0],a2']
                  isDom = dom g 1 s1Indices
                  a2' = filter (not . isDom) [0..(n2-1)]
                  v = length a2' < x2 || existsDom g 0 a2'Indices s1
              in (if v then [] else (allS2 s1 a2' x2)) ++ (iter1 (next c) x2)
          allS2 s1 a2 x2 = (iter2 (Just (choose (length a2) x2)))
              where iter2 Nothing = []
                    iter2 (Just c) = let s2 = map (a2 !!) (elems c)
                                         s2Indices = mkIndices g [[0],s2]
                                     in if existsDom g 0 s2Indices s1
                                        then iter2 (next c)
                                        else (s1,s2):(iter2 (next c))

{-
  Returns a list of support-size profiles in the order that they are used
  by the n-player algorithm (see the top "Algorithm 2").

  Let a and b be the sizes of player 1 and 2's strategy profiles, respectively.
  This function return all possible pairs of (a,b), ordered first by |a - b|,
  and second by (a+b).

  The first and second arguments are the number of actions for
  players 1 and 2, respectively.
 -}
supportSizeProfiles2p :: Int -> Int -> [(Int,Int)]
supportSizeProfiles2p a b = iter 1 1 ++ byAbsDiff 1
    where byAbsDiff d | d >= (max a b) = []
                      | otherwise = merge (iter (d+1) 1) (iter 1 (d+1)) ++
                                    byAbsDiff (d+1)
          merge xs [] = xs
          merge [] ys = ys
          merge (x:xs) (y:ys) = (x:y:(merge xs ys))
          iter x y | x > a || y > b = []
                   | otherwise = (x,y):(iter (x+1) (y+1))

{- 
   Implements the Iterated Removal of Strictly Dominated Strategies
   procedure defined as "Procedure 2" in the paper.

   Variables:
     * g = the (normalized) game
     * sis = Supports that have already been instantiated.
             As such, we just need to check whether any action in any of them
             is dominated, because removing that action would invalidate
             the support profile that we are building up.
     * ais = Non-dominated actions for the players whose supports have
             not yet been instantiated.  The actions in these supports
             are the ones we are looking to remove.  The process is iterated,
             because if we remove an action for some player, then that may
             cause an action of another player to become newly-dominated.
             Note that the lengths of sis and ais sum to the number of players.
     * ss = list of support sizes, which allows us to recognize when
            we have removed so many actions for a player that we will not
            be able to instantiate any strategy profiles.
 -}
irsds :: Game -> [Support] -> [Support] -> [Int] -> Maybe [Support]
irsds g xs ais ss = case check 0 >> iter (length xs) [] ais of
                    Nothing -> Nothing
                    Just ais' -> if ais == ais'
                                 then Just ais
                                 else irsds g xs ais' ss
    where iter _ ys [] = Just ys
          iter p ys (ai:ais) =
              let sis = mkIndices g $ xs ++ ys ++ ([0]:ais)
                  ai' = filter (not . (dom g p sis)) ai
              in if length ai' < ss !! p
                 then Nothing
                 else iter (p+1) (ys ++ [ai']) ais
          check p | p >= length xs - 1 = Just ()
                  | otherwise = let indices = mkIndices g $ inject p (xs ++ ais)
                              in if existsDom g p indices (xs !! p)
                                 then Nothing
                                 else check (p+1)
          -- construct the hacked-up support profile for use in mkIndices,
          -- as documented at the top of this file
          inject p si = take p si ++ [[0]] ++ drop (p+1) si
           
{-
  Returns the ordered list of supports for which the feasibility program
  is run for the n-player algorithm (see "Algorithm 2" in the paper
  for more details).
 -}
supportsNp :: Game -> [SupportProfile]
supportsNp g = concat $ map ofSize $ supportSizeProfilesNp $ numActions g
    where ofSize ss = let ais = map (\x -> [0..x-1]) (numActions g)
                      in case irsds g [] ais ss of
                           Nothing -> []
                           Just ais' -> recur 0 [] ais' ss
          n = length $ numActions g
          recur :: Int -> [Support] -> [Support] -> [Int] -> [SupportProfile]
          recur p xs [] _ | p == (length (players g)) = [xs]
          recur p xs (ai:ais) ss = iter (Just (choose (length ai) (ss !! p)))
              where iter Nothing = []
                    iter (Just c) = let si = map (ai !!) (elems c)
                                        xs' = xs ++ [si]
                                    in case irsds g xs' ais ss of
                                         Nothing -> iter (next c)
                                         Just ais' -> recur (p+1) xs' ais' ss ++
                                                      iter (next c)

{-
  Returns a list of support-size profiles in the order that they are used
  by the n-player algorithm (see the top "Algorithm 2")
-}
supportSizeProfilesNp :: [Int] -> [[Int]]
supportSizeProfilesNp as =
    concat $ map bySizeDiff $
               [(size, diff) | size <- [n..sum as], diff <- [0..maxDiff]]
    where n = length as
          maxDiff = maximum as - 1
          bySizeDiff :: (Int,Int) -> [[Int]]
          bySizeDiff (size, diff) = iter as
              where iter :: [Int] -> [[Int]]
                    iter [] = [[]]
                    iter (a:as) = filter possible $
                                  [(s:ss) | s <- [1..a], ss <- iter as]
                    possible ss = let rem = n - length ss
                                      lbSize = sum ss + rem
                                      lbDiff = maximum ss - minimum ss
                                      cmp = if rem == 0 then (==) else (<=)
                                  in lbSize `cmp` size && lbDiff `cmp` diff
                           