{-# LANGUAGE OverloadedStrings #-}
module Main where
import qualified Data.Text.IO as T.IO
import Data.IntSet(IntSet)
import qualified Data.IntSet as IS
import Data.IntMap.Strict(IntMap)
import qualified Data.IntMap.Strict as IM
import Data.Attoparsec.Text hiding (take)
import Data.Either(fromRight)
import Data.Char(isNumber)
import Data.List(foldl')

both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)

cards :: Parser (IntSet, IntSet)
cards = fmap (both IS.fromList) $ (,)
    <$> ("Card" *> skipSpace *> skipWhile isNumber *> ":" *> skipSpace *> decimal `sepBy` skipSpace)
    <*> (" |" *> skipSpace *> decimal `sepBy` skipSpace)

parseInput :: Parser [(IntSet, IntSet)]
parseInput = cards `sepBy` endOfLine

readInput :: IO [(IntSet, IntSet)]
readInput = fromRight (error "parse error") . parseOnly parseInput <$> T.IO.readFile "input/day4.txt"

part1 :: [(IntSet, IntSet)] -> Int
part1 cs = sum $ (2^) . pred <$> filter (/= 0) (winningNumbers cs)

winningNumbers :: [(IntSet, IntSet)] -> [Int]
winningNumbers = fmap (IS.size . uncurry IS.intersection)

-- Create an IntMap corresponding to how many of each card we have, then
-- fold over the list of how many winning numbers we have on each card
-- adding the new cards to our count map as we go.
part2 :: [(IntSet, IntSet)] -> Int
part2 cs = 
    let nums = winningNumbers cs
        startingMap = IM.fromList $ zip [1..length cs] (repeat 1)
        addNewCards :: IntMap Int -> (Int, Int) -> IntMap Int
        addNewCards cardCounts (wins, index) = 
            let numCards = IM.findWithDefault 0 index cardCounts
            in foldr (IM.adjust (+ numCards)) cardCounts [index+1..index+wins]
    in sum $ foldl' addNewCards startingMap $ zip nums [1..]

main :: IO ()
main = do
    input <- readInput
    putStrLn $ "Part 1 Solution: " ++ show (part1 input)
    putStrLn $ "Part 2 Solution: " ++ show (part2 input)