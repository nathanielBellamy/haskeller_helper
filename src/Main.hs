module Main
 (
    main
 ) where

import System.Environment (getArgs)
import Data.List
import Primes.Primes

main :: IO()
main = do
  args <- getArgs
  handle args

handle args = do
  case args of
    ["-p", x]  -> primes x
    ["-h"]     -> printHelp
    _          -> putStrLn "ʎ✋ Haskeller Helper ʎ✋ \n use -h for help "

printHelp = putStrLn "TODO: Make Help Section"
