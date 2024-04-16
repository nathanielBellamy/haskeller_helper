module Src.Notes.Item (
  Item,
  itemSerialize,
  itemDeserialize
) where

import Src.Util.StringHelper (hhSplit, hhSplitMarker)

data Item = Item { id :: Int
                 , body :: String }

itemDeserialize :: String -> Maybe Item
itemDeserialize [] = Nothing
itemDeserialize xs = Just $ Item idNumber $ (tail . concat) pieces
  where
    pieces = hhSplit xs
    idNumber = read (head pieces) :: Int

itemSerialize :: Item -> String
itemSerialize (Item id body) = "id" ++ hhSplitMarker ++ show id ++ hhSplitMarker ++ body

instance Show Item where
  show (Item id body) = show ("Item: {id: " ++ show id ++ ", body: " ++ body ++ " }")
