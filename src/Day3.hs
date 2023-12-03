module Main where
import qualified Data.Map.Strict as M
import Data.Char(isDigit)
import Data.Maybe(mapMaybe)

readInput :: IO (M.Map (Int, Int) Char)
readInput = makeMap . lines <$> readFile "input/day3.txt"

-- Note: our keys are (y, x) in order for lexicographic ordering to be row major
makeMap :: [[Char]] -> M.Map (Int, Int) Char
makeMap ls = M.fromList $
    filter ((/= '.') . snd) $ concatMap (\(y, row) -> zip (zip (repeat y) [0..]) row) $ zip [0..] ls

-- When we find a number, we keep track of:
--  - Where it was in the grid
--  - How long it is
--  - Its Value
type FoundNumber = ((Int, Int), Int, Int)

findNumbers :: M.Map (Int, Int) Char -> [FoundNumber]
findNumbers = findNumbers' (0,0) Nothing . M.toAscList . M.filter (isDigit)

findNumbers' :: (Int, Int) -- The last location that was visited
             -> Maybe FoundNumber -- a number in the process of being decoded
             -> [((Int, Int), Char)] -- The list sorted in lexicograpic order by keys
             -> [FoundNumber]
findNumbers' _ Nothing [] = []
findNumbers' _ (Just num) [] = [num]
findNumbers' _ Nothing (((y, x), c):rest) = findNumbers' (y, x) (Just ((y, x), 1, read [c])) rest
findNumbers' (prevy, prevx) (Just foundNum@((numy, numx), len, num)) (((y, x), c):rest)
    |  x == prevx+1 
    && y == prevy = findNumbers' (y, x) (Just ((numy, numx), len+1, num*10+read[c])) rest
    | otherwise   = foundNum : findNumbers' (y, x) (Just ((y, x), 1, read[c])) rest

generateAdjacentIndices :: (Int, Int) -> Int -> [(Int, Int)]
generateAdjacentIndices (y, x) len = [(y', x') | y' <- [y-1..y+1], x' <- [x-1..x+len]]

isPartNumber :: M.Map (Int, Int) Char -> FoundNumber -> Bool
isPartNumber m (index, len, _) = 
    not . null $ filter (not . isDigit) $ mapMaybe (flip M.lookup m) $ generateAdjacentIndices index len 

part1 :: M.Map (Int, Int) Char -> Int
part1 m = sum $ fmap (\(_,_,n) -> n) $ filter (isPartNumber m) $ findNumbers m

-- Find the gear ratio for a gear, if it exists
gearRatio:: [FoundNumber] -> (Int, Int) -> Maybe Int
gearRatio nums gearLoc = 
    let numberNextToGear :: (Int, Int) -> FoundNumber -> Bool
        numberNextToGear (geary, gearx) ((numy, numx), len, _) = 
            geary >= numy -1 && geary <= numy+1 && gearx >= numx-1 && gearx <= numx+len
        adjacentNums = filter (numberNextToGear gearLoc) nums
    in case adjacentNums of
        (_,_,num1):(_,_,num2):[] -> Just (num1 * num2)
        _                        -> Nothing

part2 :: M.Map(Int, Int) Char -> Int
part2 m =   
    let numbers = findNumbers m
    in sum $ mapMaybe (gearRatio numbers) $ (M.keys $ M.filter (== '*') m)

main :: IO ()
main = do
    input <- readInput
    putStrLn $ "Part 1 Solution: " ++ show (part1 input)
    putStrLn $ "Part 2 Solution: " ++ show (part2 input)