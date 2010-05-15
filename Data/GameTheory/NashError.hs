module Data.GameTheory.NashError where
{-
 - Adapted from the excellent tutorial:
 - http://halogen.note.amherst.edu/~jdtang/scheme_in_48/tutorial/overview.html
 -}

import Control.Monad.Error

data NashError = ParserError String
               | SolverError String
               | Default String
               
instance Show NashError where
    show (ParserError msg) = "Parse Error: " ++ msg
    show (SolverError msg) = "Solver Error: " ++ msg
    show (Default msg) = "Error: " ++ msg

instance Error NashError where
    noMsg  = Default "An unknown error has occurred"
    strMsg = Default

type ThrowsError = Either NashError

trapError = (flip catchError) (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

runThrows :: ThrowsError String -> String
runThrows action = extractValue $ trapError action
