import           Data.Char                 (isPunctuation, isSpace, toLower)
import           Data.Text                 as T
import           Lib
import           Test.QuickCheck
import           Test.QuickCheck.Instances

assert :: Bool -> String -> String -> IO ()
assert test passStatement failStatement =
  if test
    then putStrLn passStatement
    else putStrLn failStatement

prop_punctuationInvariant text = preProcess text == preProcess noPuncText
  where
    noPuncText = T.filter (not . isPunctuation) text

prop_whitespaceInvariant text = preProcess text == preProcess noPuncText
  where
    noPuncText = T.filter (not . isSpace) text

prop_capitalizationInvariant text = preProcess text == preProcess noPuncText
  where
    noPuncText = T.toLower text

prop_reverseInvariant text = palindrome == reversePalindrome
  where
    palindrome = isPalindrome text
    reversePalindrome = (isPalindrome . T.reverse) text

main :: IO ()
main = do
  putStrLn "Running tests..."
  quickCheckWith stdArgs {maxSuccess = 1000} prop_punctuationInvariant
  quickCheckWith stdArgs {maxSuccess = 1000} prop_whitespaceInvariant
  quickCheckWith stdArgs {maxSuccess = 1000} prop_capitalizationInvariant
  quickCheckWith stdArgs {maxSuccess = 1000} prop_reverseInvariant
  putStrLn "done!"
