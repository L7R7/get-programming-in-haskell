import           Data.Char                 (isPunctuation)
import           Lib
import           Test.QuickCheck
import           Test.QuickCheck.Instances
import Data.Text as T

assert :: Bool -> String -> String -> IO ()
assert test passStatement failStatement =
  if test
    then putStrLn passStatement
    else putStrLn failStatement

prop_punctuationInvariant text = preProcess text == preProcess noPuncText
  where
    noPuncText = T.filter (not . isPunctuation) text

prop_reverseInvariant text = palindrome == reversePalindrome
  where
    palindrome = isPalindrome text
    reversePalindrome = (isPalindrome . reverse) text

main :: IO ()
main = do
  putStrLn "Running tests..."
  quickCheckWith stdArgs {maxSuccess = 1000} prop_punctuationInvariant
  quickCheckWith stdArgs {maxSuccess = 1000} prop_reverseInvariant
  putStrLn "done!"
