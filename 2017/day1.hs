import System.IO

solveDay1Part1 = do
    contents <- readFile "2017/day1_input.txt"
    print (captcha (read contents))

solveDay1Part2 = do
    contents <- readFile "2017/day1_input.txt"
    print (captcha2 (read contents))

captcha :: Integer -> Integer
captcha nums =
    sum [if x1 == x2 then x1 else 0 | (x1, x2) <- consecutivePairs (concatHead (digs nums))]

captcha2 :: Integer -> Integer
captcha2 nums =
    sum [if x1 == x2 then x1 else 0 | (x1, x2) <- consecutiveHalfwayPairs (concatHead (digs nums))]

consecutivePairs :: [a] -> [(a, a)]
consecutivePairs x = zip x (tail x)

consecutiveHalfwayPairs :: [a] -> [(a, a)]
consecutiveHalfwayPairs x = zip x (rotate (((length x) `div` 2) - 1) (tail x))

concatHead :: [a] -> [a]
concatHead list = list ++ [head list]

-- Splits a number into its digits
-- See https://stackoverflow.com/questions/3963269/split-a-number-into-its-digits-with-haskell
digs :: Integral x => x -> [x]
digs 0 = []
digs x = digs (x `div` 10) ++ [x `mod` 10]

-- Rotates a list by a certain number of elements
-- See https://stackoverflow.com/questions/16378773/rotate-a-list-in-haskell
rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = zipWith const (drop n (cycle xs)) xs
