import System.Environment (getArgs)
import Control.Monad
import qualified Data.ByteString.Char8 as B
    
import Data.GameTheory.Nfg
import Data.GameTheory.Solver2p
import Data.GameTheory.SolverNp
import Data.GameTheory.DotNfgParser
import Data.GameTheory.NashError

-- Takes a single argument of a file containing a normal form game
-- described using the .nfg format.  Returns a sample Nash equilibrium.
-- See README for more details
main :: IO ()
main = do args <- getArgs
          input <- B.readFile $ head args
          putStrLn $ runThrows $ liftM show $ solve input
    where solve input = do g <- parseDotNfg input
                           if (length (players g)) == 2
                             then solve2p g
                             else solveNp g
