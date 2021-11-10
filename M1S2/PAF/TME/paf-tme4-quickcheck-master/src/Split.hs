module Split where

split :: Char -> String -> [String]
split c s = case break (==c) s 
    of 
  (ls, "") -> [ls]
  (ls, x:rest) -> ls : split c rest   
-- >>> split ';' ";"
-- ["",""]
unsplit :: Char -> [String] -> String
unsplit c s = case s of
  [ls] -> ls
  (ls : rest) -> ls <> [c] <> unsplit c rest
-- >>> unsplit '/' ["aa","bb","ccc","dd d"]
-- "aa/bb/ccc/dd d"

prop_split_unsplit :: Char -> String -> Bool
prop_split_unsplit c str = unsplit c (split c str) == str
 
