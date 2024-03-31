import System.Process

import System.Environment (getArgs)

main = do
  args <- getArgs
  callCommand ("ghc -o bin/hh src/Main.hs " ++ (concat args))
