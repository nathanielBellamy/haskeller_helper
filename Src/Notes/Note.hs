module Src.Notes.Note (
  -- noteDeserialize
  noteAddItem,
  noteTitleSerialize
) where

import System.Directory (createDirectoryIfMissing, doesPathExist)
import Src.Util.StringHelper (hhSplit, hhSplitMarker)
-- import Data.DateTime

import Src.Notes.Item (Item, itemDeserialize, itemSerialize)
import Src.Util.DirectoryStructure (notesDir)

data Note = Note { title :: String
                 , updatedAt :: Data.DateTime
                 , items :: [Item] }

noteDeserialize :: IO String -> Maybe Note
noteDeserialize [] = Nothing
noteDeserialize xs =  do
  let noteLines = lines xs
  let title = (titleDeserialize . head) xs
  let updatedAt = read ((head . tail) noteLines) :: DateTime
  let items = map itemDeserialize $ (tail . tail) noteLines
  Just Note title updateAt items

noteTitleSerialize :: String -> String
noteTitleSerialze xs = "HH" ++ hhSplitMarker ++ "note" ++ hhSplitMarker

titleDeserialize :: String -> String
titleDeserialze xs = (head . tail . tail) (hhSplit xs)

noteAddItem :: String -> String -> IO ()
noteAddItem fileName itemInput =
  let filePath = notesDir ++ "/" ++ fileName ++ ".txt"
      onLoadSuccess = appendFile filePath $ "\n" ++ "2" ++ hhSplitMarker ++ itemInput
  in do
    pathExists <- doesPathExist(filePath)
    case pathExists of
      True  -> onLoadSuccess
      False -> putStrLn("Could not find note: " ++ fileName)
