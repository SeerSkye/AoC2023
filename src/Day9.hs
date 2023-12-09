module Main where
import Data.Ratio
import Data.List(unfoldr)

readInput :: IO [[Int]]
readInput = fmap (fmap read . words) . lines <$> readFile "input/day9.txt"

extendByDifference :: [Int] -> [Int]
extendByDifference lst = 
    let firstDifferences = zipWith (-) (drop 1 lst) lst
    in if all (== 0) lst
        then lst ++ [0]
        else lst ++ [last lst + last (extendByDifference firstDifferences)]

part1 :: [[Int]] -> Int
part1 = sum . fmap (last . extendByDifference)

extendLeftByDifference :: [Int] -> [Int]
extendLeftByDifference lst = 
    let firstDifferences = zipWith (-) (drop 1 lst) lst
    in if all (== 0) lst
        then 0:lst
        else (head lst - head (extendLeftByDifference firstDifferences)):lst

part2 :: [[Int]] -> Int
part2 = sum . fmap (head . extendLeftByDifference)

main :: IO ()
main = do
    input <- readInput
    putStrLn $ "Part 1 Solution: " ++ show (part1 input)
    putStrLn $ "Part 2 Solution: " ++ show (part2 input)

-- For fun, let's code an alternate solution where we find closed-form equations
-- for the formulas. We'll do this by "integrating" the constant function we eventually find
-- into a polynomial written in terms of falling factorial powers instead of normal powers
-- (because normal powers don't play nice with the calculus of finite differences)

-- The falling factorial power x .^ n is equal to x(x-1)...(x-n+1)
(.^) :: Num a => a -> Int -> a
_ .^ 0 = 1
x .^ n = product $ take n $ iterate (\d -> d - 1) x

-- Transform a list of coefficients of a polynomial of falling powers into a new
-- list for the anti-difference: the function whos differences are the function passed
-- in and who's value at f(0) is the value passed in
antiDifference :: Integral a => Ratio a -> [Ratio a] -> [Ratio a]
antiDifference c coefficients = c : zipWith (/) coefficients [1..]

evaluateFromCoefficients :: Integral a => [Ratio a] -> Ratio a -> Ratio a
evaluateFromCoefficients coefficients x = sum $ fmap (\(c, n) -> c * (x .^ n)) (zip coefficients [0..])

findCoefficientsForSequence :: [Int] -> [Ratio Int]
findCoefficientsForSequence s =
    let differences = unfoldr (\diff -> if all (== 0) diff 
                                         then Nothing 
                                         else Just (diff, zipWith (-) (drop 1 diff) diff)) s 
    in foldr antiDifference [] $ fmap (fromIntegral . head) differences

part1' :: [[Int]] -> Ratio Int
part1' = sum . fmap (\s -> evaluateFromCoefficients 
                               (findCoefficientsForSequence s) 
                               (fromIntegral $ length s))

part2' :: [[Int]] -> Ratio Int
part2' = sum . fmap (\s -> evaluateFromCoefficients (findCoefficientsForSequence s) (-1))