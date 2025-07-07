module Bob (responseFor) where

import Data.Char (isLetter, isSpace, isUpper)

responseFor :: String -> String
responseFor xs = case () of
  _
    | null trimmed -> "Fine. Be that way!"
    | isShouting && isQuestion -> "Calm down, I know what I'm doing!"
    | isShouting -> "Whoa, chill out!"
    | isQuestion -> "Sure."
    | otherwise -> "Whatever."
  where
    trimmed = filter (not . isSpace) xs
    isShouting = all isUpper (filter isLetter trimmed) && any isUpper (filter isLetter trimmed)
    isQuestion = last trimmed == '?'