module Src.Notes.Item (
  Item (..),
  itemSerialize,
  itemDeserialize
) where

import Src.Util.StringHelper (hhSplit, hhSplitMarker)

data Item = Item { itemId  :: Int
                 , itemBody :: String }

itemDeserialize :: String -> Maybe Item
itemDeserialize [] = Nothing
itemDeserialize xs = Just $ Item idNumber body
  where
    pieces = hhSplit xs
    body = (concat . tail) pieces
    idNumber = read (head pieces) :: Int

itemSerialize :: Item -> String
itemSerialize (Item itemId itemBody) = (show itemId) ++ hhSplitMarker ++ itemBody

instance Show Item where
  show (Item itemId itemBody) = " " ++ (show itemId) ++ ": " ++ itemBody
