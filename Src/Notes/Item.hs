module Src.Notes.Item (
  Item,
  fromString
) where

data Item = Item { id :: Int
                 , body :: String }

splitWhen :: (Char -> Bool) -> String -> [String]
splitWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : splitWhen p s''
                            where (w, s'') = break p s'

fromString :: String -> Maybe Item
fromString [] = Nothing
fromString xs = Just $ Item idNumber $ (tail . concat) pieces
  where
    pieces = splitWhen (==':') xs
    idNumber = read (head pieces) :: Int

instance Show Item where
  show (Item id body) = show ("Item: {id: " ++ show id ++ ", body: " ++ body ++ " }")
