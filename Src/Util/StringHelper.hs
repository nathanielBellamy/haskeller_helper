module Src.Util.StringHelper (
  hhSplit
) where

hhSplitMarker :: String
hhSplitMarker = ['λ']

hhSplit :: String -> [String]
hhSplit str = splitWhen (==hhSplitMarker) str

splitWhen :: (Char -> Bool) -> String -> [String]
splitWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : splitWhen p s''
                            where (w, s'') = break p s'
