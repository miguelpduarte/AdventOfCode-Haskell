import System.IO

parseLine :: String -> [Integer]
parseLine line = map readInt (words line)

readInt :: String -> Integer
readInt = read

solveDay2Part1 = do
    contents <- readFile "2017/day2_input.txt"
    let spreadsheet = map parseLine (lines contents)
    print (spreadsheetChecksum spreadsheet checksum)

solveDay2Part2 = do
    contents <- readFile "2017/day2_input.txt"
    let spreadsheet = map parseLine (lines contents)
    print (spreadsheetChecksum spreadsheet checksum2)

spreadsheetChecksum :: [[Integer]] -> ([Integer] -> Integer) -> Integer
spreadsheetChecksum spreadsheet checksum =
    sum (map checksum spreadsheet)

checksum :: [Integer] -> Integer
checksum line =
    (maximum line) - (minimum line)

checksum2 :: [Integer] -> Integer
checksum2 line =
    (max n1 n2) `div` (min n1 n2)
    where (n1, n2) = checksum2_aux line

checksum2_aux :: [Integer] -> (Integer, Integer)
checksum2_aux [] = (0,0)
checksum2_aux (h:t)
    | divisible = (h, num)
    | otherwise = checksum2_aux t
    where (divisible, num) = findEvenlyDivisibles h t

isDivisible :: Integer -> Integer -> Bool
isDivisible a b =
    a `mod` b == 0 || b `mod` a == 0

findEvenlyDivisibles :: Integer -> [Integer] -> (Bool, Integer)
findEvenlyDivisibles num list
    | null divisibles = (False, 0)
    | otherwise = (True, divisibles!!0)
    where divisibles = filter (isDivisible num) list
