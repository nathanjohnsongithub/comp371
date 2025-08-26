-- Define an infinite list of Fibonacci numbers
fibonacci :: [Integer]
fibonacci = 0 : 1 : zipWith (+) fibonacci (tail fibonacci)

-- Get the first n Fibonacci numbers
takeFibonacci :: Int -> [Integer]
takeFibonacci n = take n fibonacci

-- Main function
main :: IO ()
main = do
    putStrLn "Enter the number of Fibonacci numbers to display:"
    input <- getLine
    let n = read input :: Int
    print (takeFibonacci n)
