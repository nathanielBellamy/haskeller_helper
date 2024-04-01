module Src.Notes.Notes (
  noteAddItem,
  notes
) where

import System.Directory (createDirectoryIfMissing, doesPathExist)
import System.FilePath.Posix (takeDirectory)

notesDir :: String
notesDir = "~/.hh/notes"

notes :: String -> IO ()
notes fileName = do
  contents <- createLoadFile fileName
  (printItems . lines) contents

printItems :: [String] -> IO ()
printItems []     = putStrLn("      ")
printItems (x:xs) = do
  putStrLn x
  printItems xs

createLoadFile :: String -> IO String
createLoadFile fileName =
  let filePath = notesDir ++ "/" ++ fileName ++ ".txt"
  in do
    pathExists <- doesPathExist(filePath)
    case pathExists of
      False -> createNoteFile filePath fileName
      True -> readFile filePath

createNoteFile :: String -> String -> IO String
createNoteFile filePath fileName =
  let
    header = ("-- " ++ filePath ++ "\n-- HH NOTE: " ++ fileName)
  in do
    createDirectoryIfMissing True (takeDirectory filePath)
    writeFile filePath header
    readFile filePath

noteAddItem :: String -> String -> IO ()
noteAddItem fileName item =
  let filePath = notesDir ++ "/" ++ fileName ++ ".txt"
  in do
    pathExists <- doesPathExist(filePath)
    case pathExists of
      True  -> appendFile filePath $ "\n" ++ item
      False -> putStrLn("Could not find note: " ++ fileName)
