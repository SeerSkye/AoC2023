{-# Language OverloadedStrings #-}
module Main where
import Data.Attoparsec.Text
import qualified Data.Text.IO as T.IO
import Data.Either(fromRight)

parseInput :: Parser [(Int, Int)]
parseInput = zip 
    <$> ("Time:" *> skipSpace *> decimal `sepBy` skipSpace <* endOfLine)
    <*> ("Distance:" *> skipSpace *> decimal `sepBy` skipSpace)

readInput :: IO [(Int, Int)]
readInput = fromRight (error "parse error") . parseOnly parseInput <$> T.IO.readFile "input/day6.txt"

{- 
Okay, let's setup the problem. For a given time t, when we 
hold the button for x seconds, our final time ends up looking like

x(t - x)

We're looking for all the integer values of x where this is higher than
some record r. Lets find the intersection of this function with r

r = x(t - x)
0 = -x^2 + tx - r

using the quadratic formula:

x = (-t +- sqrt(t^2 - 4r))/(-2)
-}

countWins :: (Int, Int) -> Int
countWins (t, r) = 
    let floatT = fromIntegral t
        floatR = fromIntegral r
        discriminantSqrt :: Double
        discriminantSqrt = sqrt $ floatT * floatT - 4 * floatR
        root1 = (- floatT + discriminantSqrt)/(-2)
        root2 = (- floatT - discriminantSqrt)/(-2)
    -- In order to handle when the answer is exact, we round to include the worse/
    -- tying values outside the one's we're looking for, and then subtract them from
    -- the count
    in ceiling root2 - floor root1 - 1

part1 :: [(Int, Int)] -> Int
part1 = product . fmap countWins

convertToPart2 :: [(Int, Int)] -> (Int, Int)
convertToPart2 = (\(s1, s2) -> (read s1, read s2)) 
    . foldr (\(t, r) (s1, s2) -> ((show t) ++ s1, (show r) ++ s2)) ("", "")

part2 :: [(Int, Int)] -> Int
part2 = countWins . convertToPart2

main :: IO ()
main = do
    input <- readInput
    putStrLn $ "Part 1 Solution: " ++ show (part1 input)
    putStrLn $ "Part 2 Solution: " ++ show (part2 input)