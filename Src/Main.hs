module Main
 (
    main
 ) where

import System.Environment (getArgs)
import Data.List
import Src.Primes.Primes
import Src.Notes.Notes

main :: IO()
main = do
  args <- getArgs
  handle args

handle args = do
    case args of
      -- primes
      ["-p", x]     -> primes x

      -- notes
      ["-n", x]     -> notes x
      ["-na", x, y] -> noteAddItem x y

      -- default
      ["-h"]        -> printHelp
      _             -> printDefault

printDefault =
  let msg = "ʎ✋ Haskeller Helper ʎ✋ \n You may have arrived here due to unrecognized input pattern. \n Use -h for help "
  in do putStrLn msg

printHelp = putStrLn "TODO: Make Help Section"
