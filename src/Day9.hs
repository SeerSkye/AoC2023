module Main where

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