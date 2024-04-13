module Src.Notes.Item (
  Item,
  itemFromString
) where

data Item = Item { id :: Int
                 , body :: String }

splitWhen :: (Char -> Bool) -> String -> [String]
splitWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : splitWhen p s''
                            where (w, s'') = break p s'

itemFromString :: String -> Maybe Item
itemFromString [] = Nothing
itemFromString xs = Just $ Item idNumber $ (tail . concat) pieces
  where
    pieces = splitWhen (=='ʎ') xs
    idNumber = read (head pieces) :: Int

instance Show Item where
  show (Item id body) = show ("Item: {id: " ++ show id ++ ", body: " ++ body ++ " }")
