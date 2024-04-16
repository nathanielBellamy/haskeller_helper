module Src.Util.StringHelper (
  hhSplit,
  hhSplitMarker
) where

hhSplitMarker :: String
hhSplitMarker = ['λ']

hhSplit :: String -> [String]
hhSplit str = splitWhen (==(head hhSplitMarker)) str

splitWhen :: (Char -> Bool) -> String -> [String]
splitWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : splitWhen p s''
                            where (w, s'') = break p s'
