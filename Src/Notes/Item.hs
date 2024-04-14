module Src.Notes.Item (
  Item,
  itemSerialize,
  itemDeserialize
) where

data Item = Item { id :: Int
                 , body :: String }

splitWhen :: (Char -> Bool) -> String -> [String]
splitWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : splitWhen p s''
                            where (w, s'') = break p s'

itemDeserialize :: String -> Maybe Item
itemDeserialize [] = Nothing
itemDeserialize xs = Just $ Item idNumber $ (tail . concat) pieces
  where
    pieces = splitWhen (=='λ') xs
    idNumber = read (head pieces) :: Int

itemSerialize :: Item -> String
itemSerialize (Item id body) = "idλ" ++ show id ++ "λ" ++ body

instance Show Item where
  show (Item id body) = show ("Item: {id: " ++ show id ++ ", body: " ++ body ++ " }")
