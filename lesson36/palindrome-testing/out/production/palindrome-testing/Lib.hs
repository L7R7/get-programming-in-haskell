module Lib
  ( isPalindrome
  , preProcess
  ) where

import           Data.Char (isPunctuation)
import           Data.Text as T

isPalindrome :: T.Text -> Bool
isPalindrome text = cleanText == T.reverse cleanText
  where
    cleanText = preProcess text

preProcess :: T.Text -> T.Text
preProcess = T.filter (not . isPunctuation)
