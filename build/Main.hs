import System.Process

import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)


main = do
  args <- getArgs
  createDirectoryIfMissing True "bin"
  callCommand ("ghc -o bin/hh Src/Main.hs " ++ (concat args))

