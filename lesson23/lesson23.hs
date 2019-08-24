{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T

firstWord :: String
firstWord = "pessimism"

secondWord :: T.Text
secondWord = T.pack firstWord

thirdWord :: String
thirdWord = T.unpack secondWord

fourthWord :: T.Text
fourthWord = T.pack thirdWord

aWord :: T.Text
aWord = "Cheese"

sampleInput :: T.Text
sampleInput = "this\nis a\nsample input"

lines :: [T.Text]
lines = T.lines sampleInput

words :: [T.Text]
words = T.words sampleInput

breakText :: T.Text
breakText = "simple"

exampleText :: T.Text
exampleText = "This is simple to do"

splitted :: [T.Text]
splitted = T.splitOn breakText exampleText

combinedTextMonoid :: T.Text
combinedTextMonoid = mconcat ["some", " ", "text"]

combinedTextSemigroup :: T.Text
combinedTextSemigroup = "some" <> " " <> "text"

mLines :: T.Text -> [T.Text]
mLines = T.splitOn "\n"

mUnlines :: [T.Text] -> T.Text
mUnlines = T.intercalate "\n"
