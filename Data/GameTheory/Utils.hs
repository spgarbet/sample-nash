module Data.GameTheory.Utils (readDouble) where

import qualified Char as C

{- This is a special-purpose replacement for "read :: Double", for use
   with the parser.  It assumes no whitespace.  It has been tested using
   QuickCheck on strings created by calling "show" on random doubles.
 -}
readDouble :: String -> Double
readDouble ('-':s) = -(readDouble s)
readDouble s = iter1 s 0
    where -- iter1 parses everything before '.', 'e', or 'E',
          -- accumulating its result in the second argument.
          -- at which point it passes control to either 'iter2' or 'emit'.
          iter1 :: [Char] -> Int -> Double
          iter1 [] v = emit v 0.0
          iter1 (c:cs) v | c >= '0' && c <= '9' = iter1 cs (app v c)
                         | c == '.'             = iter2 cs v 0.0
                         | c `elem` "eE"        = emit v (getExp cs)
                         | otherwise            = error "unexpected char"
          -- iter2 parses everything between '.' and either 'e' or 'E'.
          -- Like iter1, it accumulates an integer value as its second
          -- argument, but this value ignores the fact that we are now
          -- after the decimal point. To track this, it also accumulates
          -- an exponent as its third argument.  This value is then
          -- combined with any exponent specified after 'e' or 'E' to
          -- produce the final value.  For example, when parsing
          -- "1.23e45", iter2 will be eventually be called with the arguments
          -- "e45", 123, and -2.  At that point, it passes control to emit.
          iter2 [] v exp = emit v exp
          iter2 (c:cs) v exp
              | c >= '0' && c <= '9' = iter2 cs (app v c) (exp - 1.0)
              | c `elem` "eE"        = emit v (exp + getExp cs)
              | otherwise            = error "unexpected char"
          -- Parses the exponent
          getExp :: [Char] -> Double
          getExp ('-':cs) = -(getExp cs)
          getExp cs = expIter cs 0
              where expIter [] e = fromIntegral e
                    expIter (c:cs) e = expIter cs (app e c)
          -- Applies a character to the end of an integer.  For example,
          -- "app 2 '3'" evaluates to 23.  It is the caller's responsibility
          -- to ensure that the character is a digit.
          app :: Int -> Char -> Int
          app v c = v * 10 + (C.ord c - C.ord '0')
          -- Produces the final value from two variables that the
          -- above functions have accumulated: an integer and an exponent.
          emit :: Int -> Double -> Double
          emit v exp = (fromIntegral v) * (10.0 ** exp)
