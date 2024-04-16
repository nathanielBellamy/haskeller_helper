module Src.Notes.Note (
  noteAddItem,
  noteDeserialize,
  noteLoadAddItem,
  notePrint,
  noteTitleSerialize
) where

import System.Directory (createDirectoryIfMissing, doesPathExist)
import Src.Util.StringHelper (hhSplit, hhSplitMarker)
import Data.Time.Clock (getCurrentTime, UTCTime)

import Src.Notes.Item (Item(..), itemDeserialize, itemSerialize)
import Src.Util.DirectoryStructure (notePath)

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

noteSerialize :: Note -> String
noteSerialize note = ((noteTitleSerialize . title) note)
                        ++ "\n" ++ ((show . updatedAt) note)
                        ++ "\n" ++ ((itemsSerialize . items) note)

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

itemsSerialize :: [Item] -> String
itemsSerialize []    = []
itemsSerialize items = foldr func "" items
                       where func :: Item -> String -> String
                             func item str = str ++ "\n" ++ (itemSerialize item)

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

noteLoad :: String -> IO (Maybe Note)
noteLoad fileName = do
  let filePath = notePath fileName
  pathExists <- doesPathExist(filePath)
  case pathExists of
    True  -> return Nothing
    False -> do
      contents <- readFile filePath
      return (noteDeserialize contents)

noteNewItemId :: Note -> Int
noteNewItemId note = (foldr gt 0 (items note)) + 1
                     where gt :: Item -> Int -> Int
                           gt item oldMax = let iid = (itemId item) in do
                                            case (iid > oldMax) of
                                             True  -> iid
                                             False -> oldMax

noteAddItem :: Note -> String -> IO Note
noteAddItem note newItemBody = do
  updatedAt <- getCurrentTime
  let newItem = Item {itemId=(noteNewItemId note), itemBody=newItemBody}
  let newItems = (items note) ++ [newItem]
  return (Note (title note) updatedAt newItems)

noteLoadAddItem :: String -> String -> IO ()
noteLoadAddItem fileName newItemBody = do
  note <- noteLoad fileName
  case note of
    Nothing -> return ()
    Just n  -> do
      noteNew <- noteAddItem n newItemBody
      writeFile (notePath fileName) (noteSerialize noteNew)
