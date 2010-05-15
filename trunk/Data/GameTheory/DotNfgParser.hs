{-
Parses the .nfg format, which is defined in
"Gambit Software Tools for Game Theory" at
http://gambit.sourceforge.net/doc/gambit-0.2007.01.30/gambit/x1939.html
-}
{-# LANGUAGE BangPatterns #-}
module Data.GameTheory.DotNfgParser (parseDotNfg) where

import Text.ParserCombinators.Parsec hiding (Parser, GenParser)
import Text.Parsec.ByteString
import Data.Array
import qualified Data.ByteString.Char8 as B
import qualified Char as C
import Data.List (foldl')
import Control.Monad
import Control.Monad.Error

import qualified Data.GameTheory.Nfg as N
import Data.GameTheory.NashError
import Data.GameTheory.Utils

space1 = many1 space

int :: Parser Int
int = liftM read (many1 digit)

quotedString :: Parser String
quotedString = char '"' >> manyTill anyChar (char '"')
         
title :: Parser String
title = string "NFG" >> space1 >> 
        char '1' >> space1 >> 
        (char 'D' <|> char 'R') >> space1 >> -- GAMUT uses 'D'
        quotedString

-- used to parse lists of players or actions
headerList :: (Parser a) -> Parser [a]
headerList item = space1 >> between (char '{') (char '}') oneItem
    where oneItem = space1 >> endBy1 item space1

{-
Let:
   n = number of players
   c = number of payoff cells (i.e., the size of payoff matrix)

The payoffs are a whitespace-separate list of doubles.
The first n values are the payoffs to each player when
each player plays their first action.  The next n values
are for the same strategy profile, except that player 1's action
is incremented, and so on.

We parse n doubles at a time, and return a list of c elements,
each of which contains the n elements of a cell in the payoff matrix.
The later call to mkGame will convert this into the form used
in the Game data structure.

We steal the input from Parsec using getInput, so that we can
use our own function to parse doubles more quickly.
 -}
payoffMatrix :: Int -> Int -> Parser [[Double]]
payoffMatrix n c = getInput >>= (return . cells . splitInput)
    where cells :: [B.ByteString] -> [[Double]]
          cells ds = iter c ds (replicate n [])
              where iter 0 [] res = map reverse res
                    iter c ds res = let (f,r) = splitAt n ds
                                        !res' = zipWith (:) (toDoubles f) res
                                    in iter (c-1) r res'
                    toDoubles = map (readDouble . B.unpack)

splitInput = filter (not . B.null) . (B.splitWith isSpace)
    where isSpace c = C.ord c < minOrd
          -- This is a hack, which assumes that anything below what
          -- we expect in a double is a separator.  However, this is somewhat
          -- mitigated by the fact that we check that we parse the
          -- expected number of elements
          minOrd = minimum $ map C.ord "-eE0."

nfg :: Parser N.Game
nfg = do t <- title
         players <- headerList quotedString
         actions <- headerList int
         let n = length players
         when (n /= length actions) $ fail "#players != #action counts"
         let c = foldl' (*) 1 actions
         payoffs <- payoffMatrix n c
         return $ N.mkGame t players actions payoffs
    
parseDotNfg :: B.ByteString -> ThrowsError N.Game
parseDotNfg input = case parse nfg ".nfg parser" input of
                      Left err  -> throwError $ ParserError $ show err
                      Right val -> return val
