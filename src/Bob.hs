module Bob (responseFor) where

import Data.Char

responseFor :: String -> String
responseFor xs
  | null trimmedText = "Fine. Be that way!"
  | isShouting && isQuestion = "Calm down, I know what I'm doing!"
  | isShouting = "Whoa, chill out!"
  | isQuestion = "Sure."
  | otherwise = "Whatever."
  where
    trimmedText = filter (not . isSpace) xs
    onlyLetters = filter isLetter trimmedText
    isShouting = all isUpper onlyLetters && any isUpper onlyLetters
    isQuestion = last trimmedText == '?'