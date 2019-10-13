import Data.List
import Data.Maybe

input :: Integer
input = 289326

solveDay3Part1 = do
    print (getSteps input)

solveDay3Part2 = do
    print (todo input)

todo :: Integer -> Integer
todo x = x

getSteps :: Integer -> Integer
getSteps n = layer + distance_to_closest_midpoint
    where layer = whichLayer n
          midpoints = getMidpoints layer
          distance_to_closest_midpoint = minimum $ map (abs . (+ (-n))) midpoints

getMidpoints :: Integer -> [Integer]
getMidpoints layer =
    map (+ previous_layer_max) $ take 4 [layer, 3*layer..]
    where previous_layer_max = (2*(layer-1)+1)^2

whichLayer :: Integer -> Integer
whichLayer n = maybe (-1) fst $ find (\(idx, square) -> square >= n) max_corners
    where max_corners = [(x `div` 2, x^2) | x <- [1,3..]]
