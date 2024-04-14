module Src.Notes.Note (
  -- noteDeserialize
  noteAddItem
) where

import System.Directory (createDirectoryIfMissing, doesPathExist)
-- import Data.DateTime

import Src.Notes.Item (Item, itemDeserialize, itemSerialize)
import Src.Notes.Notes (notesDir)

-- data Note = Note { title :: String
--                  , updatedAt :: Data.DateTime
--                  , items :: [Item] }

-- noteDeserialize :: IO String -> Maybe Note
-- noteDeserialize [] = Nothing
-- noteDeserialize xs =  do
--   let noteLines = lines xs
--   let updatedAt = read (head noteLines) :: DateTime
--   let items = map itemDeserialize $ tail noteLines
--   Just Note "foo" updateAt items

noteAddItem :: String -> String -> IO ()
noteAddItem fileName itemInput =
  let filePath = notesDir ++ "/" ++ fileName ++ ".txt"
      onLoadSuccess = appendFile filePath $ "\n" ++ "2λ" ++ itemInput
  in do
    pathExists <- doesPathExist(filePath)
    case pathExists of
      True  -> onLoadSuccess
      False -> putStrLn("Could not find note: " ++ fileName)

