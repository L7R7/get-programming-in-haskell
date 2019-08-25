{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment
import System.IO

getCounts :: T.Text -> (Int, Int, Int)
getCounts input = (charCount, wordCount, lineCount)
  where
    charCount = T.length input
    wordCount = (length . T.words) input
    lineCount = (length . T.lines) input

countsText :: (Int, Int, Int) -> T.Text
countsText (cc, wc, lc) =
  T.pack
    (unwords ["chars: ", show cc, " words: ", show wc, " lines: ", show lc])

main :: IO ()
main = do
  args <- getArgs
  let fileName = head args
  input <- TIO.readFile fileName
  let summary = (countsText . getCounts) input
  TIO.appendFile "stats.dat" (mconcat [T.pack fileName, " ", summary, "\n"])
  TIO.putStrLn summary
