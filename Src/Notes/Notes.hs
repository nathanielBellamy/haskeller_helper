module Src.Notes.Notes (
  notes,
  notesDir
) where

import System.Directory (createDirectoryIfMissing, doesPathExist)
import System.FilePath.Posix (takeDirectory)
import Data.Time.Clock (getCurrentTime)

import Src.Notes.Item (Item, itemDeserialize, itemSerialize)
import Src.Notes.Note (noteDeserialize, notePrint, noteTitleSerialize)
import Src.Util.DirectoryStructure (notesDir)

notes :: String -> IO ()
notes fileName = do
  contents <- createLoadFile fileName
  let maybeNote = noteDeserialize contents
  case maybeNote of
    Nothing -> putStrLn $ "An Error Occurred Parsing Note: " ++ fileName
    Just n  -> notePrint n

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
    appendFile filePath $ "\n" ++ (show now)
    readFile filePath


