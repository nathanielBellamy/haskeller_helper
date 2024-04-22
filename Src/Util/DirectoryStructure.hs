module Src.Util.DirectoryStructure (
  notesDir,
  notePath
) where

notesDir :: String
notesDir = "~/.hh/notes"

notePath :: String -> String
notePath fileName = notesDir ++ "/" ++ fileName ++ ".txt"
