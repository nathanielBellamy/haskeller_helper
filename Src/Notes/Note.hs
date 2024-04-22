module Src.Notes.Note (
  noteAddItem,
  noteDeserialize,
  noteLoadAddItem,
  notePrint,
  noteTitleSerialize
) where

import Data.List (foldl', intercalate)
import Data.Time.Clock (getCurrentTime, UTCTime)
import System.Directory (createDirectoryIfMissing, doesPathExist)
import Src.Util.StringHelper (hhSplit, hhSplitMarker)

import Src.Notes.Item (Item(..), itemDeserialize, itemSerialize)
import Src.Util.DirectoryStructure (notePath)

data Note = Note { noteTitle :: String
                 , noteUpdatedAt :: UTCTime
                 , noteItems :: [Item] }

noteDeserialize :: String -> Maybe Note
noteDeserialize []           = Nothing
noteDeserialize xs           = Just $ Note title updatedAt items
  where noteLines = lines xs
        title = (noteTitleDeserialize . head) noteLines
        updatedAt = read ((head . tail) noteLines) :: UTCTime
        itemsLines = (tail . tail) noteLines
        items = case (null itemsLines) of
                True   -> []
                False  -> noteItemsDeserialize itemsLines

noteSerialize :: Note -> String
noteSerialize note = ((noteTitleSerialize . noteTitle) note)
                        ++ "\n" ++ ((show . noteUpdatedAt) note)
                        ++ "\n" ++ ((noteItemsSerialize . noteItems) note)

noteTitleSerialize :: String -> String
noteTitleSerialize title = "HH" ++ hhSplitMarker ++ "note" ++ hhSplitMarker ++ title


noteTitleDeserialize :: String -> String
noteTitleDeserialize xs = head $ (tail . tail) $ hhSplit xs

noteItemsDeserialize :: [String] -> [Item]
noteItemsDeserialize [] = []
noteItemsDeserialize xs = foldl' func [] xs
                      where func :: [Item] -> String -> [Item]
                            func items []   = items
                            func items str  = case (itemDeserialize str) of
                                                 Nothing   -> items
                                                 Just i    -> items ++ [i]

noteItemsSerialize :: [Item] -> String
noteItemsSerialize []    = []
noteItemsSerialize items = foldl' func "" items
                       where func :: String -> Item -> String
                             func str item = str ++ "\n" ++ (itemSerialize item)

notePrint :: Note -> IO ()
notePrint note = do
  putStrLn $ noteTitle note
  putStrLn $ show (noteUpdatedAt note)
  noteItemsPrint (noteItems note)

noteItemsPrint :: [Item] -> IO ()
noteItemsPrint []     = putStrLn("")
noteItemsPrint (x:xs) = do
  (putStrLn . show) x
  noteItemsPrint xs

noteLoad :: String -> IO (Maybe Note)
noteLoad fileName = do
  let filePath = notePath fileName
  pathExists <- doesPathExist(filePath)
  case pathExists of
    False  -> do
      putStrLn("Error: noteLoad filePath does not exist. Attempted to load: " ++ filePath)
      return Nothing
    True -> do
      contents <- readFile filePath
      let noteMaybe = noteDeserialize contents
      case noteMaybe of
        Just n  -> return(Just n)
        Nothing -> do
          putStrLn("Error: Unable To Deserialize Note: " ++ fileName)
          return(Nothing)

noteNewItemId :: Note -> Int
noteNewItemId note = (foldr gt 0 (noteItems note)) + 1
                     where gt :: Item -> Int -> Int
                           gt item oldMax = let iid = (itemId item) in do
                                            case (iid > oldMax) of
                                             True  -> iid
                                             False -> oldMax

noteAddItem :: Note -> String -> IO Note
noteAddItem note newItemBody = do
  updatedAt <- getCurrentTime
  let newItem = Item {itemId=(noteNewItemId note), itemBody=newItemBody}
  let newItems = (noteItems note) ++ [newItem]
  return (Note (noteTitle note) updatedAt newItems)

noteLoadAddItem :: String -> [String] -> IO ()
noteLoadAddItem fileName newItemBody = do
  note <- noteLoad fileName
  case note of
    Nothing -> putStrLn("Error: Unable To Load Note: " ++ fileName)
    Just n  -> do
      noteNew <- noteAddItem n $ intercalate " " newItemBody
      (putStrLn . show) noteNew
      writeFile (notePath fileName) (noteSerialize noteNew)


instance Show Note where
  show (Note noteTitle noteUpdatedAt noteItems) = show ("Note: {title: " ++ noteTitle
                                                          ++ ", updatedAt: " ++ (show noteUpdatedAt)
                                                          ++ ", items: " ++ (noteItemsSerialize noteItems)
                                                          ++ " }")
