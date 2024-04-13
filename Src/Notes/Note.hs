module Src.Notes.Note (
  Note,
  noteFromString
) where

import Src.Notes.Item (Item)
import Data.DateTime

data Note = Note { title :: String
                 , updatedAt :: DateTime
                 , items :: [Item] }

noteFromString :: IO String -> Maybe Note
noteFromString [] = Nothing
noteFromString xs =  do
  let noteLines = lines xs
  let updatedAt = read (head noteLines) :: DateTime
  let items = map itemFromString $ tail noteLines;

