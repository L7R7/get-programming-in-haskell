import qualified Data.Text.IO as TIO
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  let source = args !! 0
  let target = args !! 1
  input <- TIO.readFile source
  TIO.writeFile target input
  putStrLn ("Done! Copied '" ++ target ++ "' to: '" ++ target ++ "'.")
