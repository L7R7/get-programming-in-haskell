fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

main :: IO ()
main = do
  putStrLn "Hello! Which Fibonacci number do you want to know?"
  n <- getLine
  let result = fib $ read n
  putStrLn ("The result is " ++ show result)
