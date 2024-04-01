module Src.Notes.Notes (
  notes
) where

import System.Directory (createDirectoryIfMissing, doesPathExist)
import System.FilePath.Posix

notes :: String -> IO()
notes fileName = do
  contents <- loadFile fileName
  let oldNotes = lines contents
  putStrLn(head oldNotes)
  putStrLn(head ( tail oldNotes))

loadFile :: String -> IO String
loadFile fileName =
  let dirPath = "~/.hh/notes"
      filePath = dirPath ++ "/" ++ fileName ++ ".txt"
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
