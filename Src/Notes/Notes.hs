module Src.Notes.Notes (
  notesDir,
  notesRepl
) where

import System.IO
import Data.List
import System.Directory (createDirectoryIfMissing, doesPathExist)
import System.FilePath.Posix (takeDirectory)
import Data.Time.Clock (getCurrentTime)

import Src.Notes.Item (Item, itemDeserialize, itemSerialize)
import Src.Notes.Note (Note, noteDeserialize, notePrint, noteTitleSerialize)
import Src.Util.DirectoryStructure (notesDir)
import Src.Util.StringHelper (hhSplit, hhSplitMarker)

notesHandleArgs :: [String] -> [Note] -> IO [Note]
notesHandleArgs args notes = do
  case args of
    -- TODO:
    --  - ["set"], set current/working note
    --  - implement Handle for settings
    [x]         -> do
      noteLoadAndPrint x
      return(notes)
    _           -> do
      putStrLn ("Unrecognized input.")
      return(notes)

notesRepl :: [Note] -> IO ()
notesRepl notes = do
  putStrLn("==HH " ++ hhSplitMarker ++ " notes==")
  input <- notesReplRead
  let args = words input

  case args of
    [":quit"]  -> putStrLn ("====HH")
    _          -> do
      newNotes <- notesHandleArgs args notes
      notesRepl newNotes

notesReplRead :: IO String
notesReplRead = putStr "HH:notes> "
         >> hFlush stdout
         >> getLine


noteLoadAndPrint :: String -> IO ()
noteLoadAndPrint fileName = do
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


