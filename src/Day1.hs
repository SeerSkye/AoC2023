module Main where
import Data.Char(isDigit)
import Data.List(isPrefixOf, tails)
import Data.Maybe(listToMaybe, catMaybes)
import Control.Arrow((&&&))

readInput :: IO [String]
readInput = lines <$> readFile "input/day1.txt"

p1FindDigits :: String -> Maybe Int
p1FindDigits s = do
    let digitsList = fmap (read . pure) $ filter isDigit s
    firstDigit <- listToMaybe digitsList
    lastDigit <- listToMaybe $ reverse digitsList
    pure (10 * firstDigit + lastDigit)

part1 :: [String] -> Maybe Int
part1 = fmap sum . traverse p1FindDigits

p2Digits :: [(String, Int)]
p2Digits = 
    [ ("one", 1)
    , ("two", 2)
    , ("three", 3)
    , ("four", 4)
    , ("five", 5)
    , ("six", 6)
    , ("seven", 7)
    , ("eight", 8)
    , ("nine", 9)
    ] ++ fmap (show &&& id) [0..9]

prefixLookup :: String -> Maybe Int
prefixLookup s = listToMaybe $ catMaybes $ fmap (\(k, v) -> if k `isPrefixOf` s then Just v else Nothing) p2Digits

p2FindDigits :: String -> Maybe Int
p2FindDigits s =
    let digitsList = catMaybes $ prefixLookup <$> tails s
    in -- Same as part 1 but in applicative style
        (+) <$> ((10*) <$> listToMaybe digitsList) 
            <*> (listToMaybe $ reverse digitsList)

part2 :: [String] -> Maybe Int
part2 = fmap sum . traverse p2FindDigits

main :: IO ()
main = do
    input <- readInput
    putStrLn $ "Part 1 Solution: " ++ show (part1 input)
    putStrLn $ "Part 2 Solution: " ++ show (part2 input)