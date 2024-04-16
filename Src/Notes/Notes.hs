module Src.Notes.Notes (
  notes,
  notesDir
) where

import System.Directory (createDirectoryIfMissing, doesPathExist)
import System.FilePath.Posix (takeDirectory)
import Data.Map
import Data.Time.Clock (getCurrentTime)

import Src.Notes.Item (Item, itemDeserialize, itemSerialize)
import Src.Notes.Note (noteTitleSerialize)
import Src.Util.DirectoryStructure (notesDir)

notes :: String -> IO ()
notes fileName = do
  contents <- createLoadFile fileName
  (printItems . parseItems) contents

parseItems :: String -> [Maybe Item]
parseItems xs
  | (null . tail . lines) xs          = []
  | (null . tail . tail . lines) xs   = []
  | otherwise                         = map itemDeserialize itemLines
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
    writeFile filePath (noteTitleSerialize fileName)
    appendFile filePath (show now)
    readFile filePath
