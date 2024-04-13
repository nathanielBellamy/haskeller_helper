module Src.Notes.Notes (
  noteAddItem,
  notes
) where

import System.Directory (createDirectoryIfMissing, doesPathExist)
import System.FilePath.Posix (takeDirectory)
import Data.Time.Clock (getCurrentTime)

import Src.Notes.Item (Item, itemFromString)

notesDir :: String
notesDir = "~/.hh/notes"

notes :: String -> IO ()
notes fileName = do
  contents <- createLoadFile fileName
  (printItems . parseItems) contents

parseItems :: String -> [Maybe Item]
parseItems xs
  | (null . tail . lines) xs          = []
  | (null . tail . tail . lines) xs   = []
  | otherwise                         = map itemFromString itemLines
  where
    itemLines = (tail . tail . lines) xs


printItems :: [Maybe Item] -> IO ()
printItems []     = putStrLn("")
printItems (x:xs) = do
  (putStrLn . show) x
  printItems xs

createLoadFile :: String -> IO String
createLoadFile fileName =
  let filePath = notesDir ++ "/" ++ fileName ++ ".txt"
  in do
    pathExists <- doesPathExist(filePath)
    case pathExists of
      False -> createNoteFile filePath fileName
      True  -> readFile filePath

createNoteFile :: String -> String -> IO String
createNoteFile filePath fileName =
  let
  in do
    createDirectoryIfMissing True (takeDirectory filePath)
    now <- getCurrentTime
    writeFile filePath "HHʎnote\n"
    appendFile filePath (show now)
    readFile filePath

noteAddItem :: String -> String -> IO ()
noteAddItem fileName item =
  let filePath = notesDir ++ "/" ++ fileName ++ ".txt"
  in do
    pathExists <- doesPathExist(filePath)
    case pathExists of
      True  -> appendFile filePath $ "\n" ++ item
      False -> putStrLn("Could not find note: " ++ fileName)
