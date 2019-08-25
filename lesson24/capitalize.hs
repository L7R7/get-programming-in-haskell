import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  let file = args !! 0
  input <- TIO.readFile file
  TIO.writeFile file (T.toUpper input)
  putStrLn ("Done! Converted '" ++ file ++ "' to uppercase.")
