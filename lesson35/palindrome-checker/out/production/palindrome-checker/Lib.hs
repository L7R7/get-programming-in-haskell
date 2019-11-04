module Lib
  ( isPalindrome
  ) where

import           Data.Char (isPunctuation, isSpace, toLower)
import qualified Data.Text as T

someFunc :: IO ()
someFunc = putStrLn "someFunc"

stripWhiteSpace :: T.Text -> T.Text
stripWhiteSpace = T.filter (not . isSpace)

stripPunctuation :: T.Text -> T.Text
stripPunctuation = T.filter (not . isPunctuation)

preProcess :: T.Text -> T.Text
preProcess = stripWhiteSpace . stripPunctuation . T.toLower

isPalindrome :: T.Text -> Bool
isPalindrome text = cleanText == T.reverse cleanText
  where
    cleanText = preProcess text
