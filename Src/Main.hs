module Main
 (
    main
 ) where

import System.Environment (getArgs)
import System.IO
import Data.List
import Src.Primes.Primes
import Src.Notes.Notes

main :: IO()
main = do
  args <- getArgs
  handle args

handle :: [String] -> IO ()
handle args = do
    case args of
      -- repl
      ["-repl"]     -> repl

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


repl :: IO ()
repl = do
  input <- replRead
  let args = words input

  case args of
    [":quit"]  -> putStrLn ("HH-REPL> QUIT <<")
    _          -> handle args >> repl

replRead :: IO String
replRead = putStr "HH-REPL> "
         >> hFlush stdout
         >> getLine


