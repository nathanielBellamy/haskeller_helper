module Src.Notes.Note (
  noteAddItem,
  noteDeserialize,
  notePrint,
  noteTitleSerialize
) where

import System.Directory (createDirectoryIfMissing, doesPathExist)
import Src.Util.StringHelper (hhSplit, hhSplitMarker)
import Data.Time.Clock (UTCTime)

import Src.Notes.Item (Item, itemDeserialize, itemSerialize)
import Src.Util.DirectoryStructure (notesDir)

data Note = Note { title :: String
                 , updatedAt :: UTCTime
                 , items :: [Item] }

noteDeserialize :: String -> Maybe Note
noteDeserialize [] = Nothing
noteDeserialize xs =  do
  let noteLines = lines xs
  let title = (titleDeserialize . head) noteLines
  let updatedAt = read ((head . tail) noteLines) :: UTCTime
  let items = itemsDeserialize $ (tail . tail) noteLines
  Just $ Note title updatedAt items

noteTitleSerialize :: String -> String
noteTitleSerialize title = "HH" ++ hhSplitMarker ++ "note" ++ hhSplitMarker ++ title

titleDeserialize :: String -> String
titleDeserialize xs = head $ (tail . tail) $ hhSplit xs

itemsDeserialize :: [String] -> [Item]
itemsDeserialize [] = []
itemsDeserialize xs = foldr func [] xs
                      where func :: String -> [Item] -> [Item]
                            func []  items   = items
                            func str items   = case (itemDeserialize str) of
                                                 Nothing   -> items
                                                 Just i    -> i:items

notePrint :: Note -> IO ()
notePrint note = do
  putStrLn $ title note
  putStrLn $ show (updatedAt note)
  itemsPrint (items note)

itemsPrint :: [Item] -> IO ()
itemsPrint []     = putStrLn("")
itemsPrint (x:xs) = do
  (putStrLn . show) x
  itemsPrint xs

noteAddItem :: String -> String -> IO ()
noteAddItem fileName itemInput =
  let filePath = notesDir ++ "/" ++ fileName ++ ".txt"
      onLoadSuccess = appendFile filePath $ "\n" ++ "2" ++ hhSplitMarker ++ itemInput
  in do
    pathExists <- doesPathExist(filePath)
    case pathExists of
      True  -> onLoadSuccess
      False -> putStrLn("Could not find note: " ++ fileName)
