{-# Language OverloadedStrings #-}
module Main where
import Data.List(group, intercalate, (!?), inits, tails)
import Data.Attoparsec.Text(Parser, parseOnly, endOfLine, takeTill, skipSpace, decimal, sepBy)
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import Data.Either(fromRight)
import Data.Char(isSpace)
import Data.Maybe(fromMaybe)
import Data.MemoTrie(memo2)

parseLine :: Parser ([Char], [Int])
parseLine = (,) <$> (T.unpack <$> takeTill (isSpace)) <* skipSpace <*> decimal `sepBy` ","

parseInput :: Parser [([Char], [Int])]
parseInput = parseLine `sepBy` endOfLine

readInput :: IO [([Char], [Int])]
readInput = fromRight (error "parse error") . parseOnly parseInput <$> T.IO.readFile "input/day12.txt"

-- Add every possible combination of n #s into the ?s in a string
insertSprings :: Int -> [Char] -> [[Char]]
insertSprings 0 s = [s]
insertSprings n s
    | length (filter (== '?') s) < n = []
    | length (filter (== '?') s) == n = [fmap (\c -> if c == '?' then '#' else c) s]
    | otherwise = insertSprings (n - 1) (replaceFirst '?' '#' s)
                ++ insertSprings n (replaceFirst '?' '.' s)
  where
    replaceFirst :: Char -> Char -> [Char] -> [Char]
    replaceFirst _ _ [] = []
    replaceFirst toReplace newChar (c:cs) 
        | c == toReplace = newChar : cs
        | otherwise = c : replaceFirst toReplace newChar cs 

isValid :: [Int] -> [Char] -> Bool
isValid counts str = (length <$> filter (any (== '#')) (group str)) == counts

countArrangements :: [Char] -> [Int] -> Int
countArrangements str counts = 
    let springsToAdd = sum counts - length (filter (== '#') str)
    in length $ filter (isValid counts) $ insertSprings springsToAdd str

part1 :: [([Char], [Int])] -> Int
part1 strs = sum $ fmap (uncurry countArrangements) strs

badpart2 :: [([Char], [Int])] -> Int
badpart2 = part1 . fmap (\(str, counts) -> (intercalate "?" $ replicate 5 str, concat $ replicate 5 counts))
-- okay that's not gonna finish anytime soon, let's try to come up with a slightly smarter method

countArrangements' :: [Char] -> [Int] -> Int
-- There's one valid way to insert no groups into the empty string
countArrangements' [] [] = 1
-- There's no valid way to insert some number of groups into the empty string
countArrangements' [] _ = 0
-- If we have no more groups of springs to insert,
-- we're good so long as the remaining string does not
-- contain any '#' characters
countArrangements' str [] 
    | any (== '#') str = 0
    | otherwise = 1
countArrangements' str (count:groups) =
    let -- The number of characters that must remain after inserting the first group,
        -- in order to insert the rest of the groups
        minimumRemainingChars = sum groups + length groups - 1
        canInsertIntoStart :: [Char] -> Int -> Bool
        canInsertIntoStart s groupSize = all (\c -> c == '?' || c == '#') (take groupSize s)
                           && (fromMaybe False (fmap (\c -> c == '?' || c == '.') (s !? groupSize))
                              || length s == groupSize)
        possibleContinuations :: [Char] -> Int -> [[Char]]
        possibleContinuations [] _ = []
        possibleContinuations s@(c:cs) groupSize 
            | length s < (groupSize + minimumRemainingChars + 1) = []
            | canInsertIntoStart s groupSize = case c of
                '#' -> [drop (groupSize + 1) s]
                _   -> drop (groupSize + 1) s : possibleContinuations cs groupSize
            | otherwise = case c of
                '#' -> []
                _ -> possibleContinuations cs groupSize
    in sum $ fmap (\s -> countArrangements' s groups) (possibleContinuations str count)

-- I tried to make this faster by extrapolating from just one expansion,
-- this works on the sample but doesn't work on the actual input, I restricted
-- it to only try to do it if there's an even divisor, but that's almost certainly
-- not actually correct either
stillBadpart2 :: [([Char], [Int])] -> Int
stillBadpart2 input = sum $ fmap extrapolateIfPossible input
  where
    extrapolateIfPossible (str, counts) = 
        let unexpandedCount = countArrangements' str counts
            oneExpansionCount = uncurry countArrangements' $ expandOnce (str, counts)
        in if oneExpansionCount `mod` unexpandedCount == 0
            then (oneExpansionCount `div` unexpandedCount) ^ 4 * unexpandedCount
            else uncurry countArrangements' $ expandFiveTimes (str, counts)
    expandOnce (str, counts) = (intercalate "?" $ replicate 2 str, concat $ replicate 2 counts)
    expandFiveTimes (str, counts) = (intercalate "?" $ replicate 5 str, concat $ replicate 5 counts)

-- Okay even this is too slow, let's really analyze the problem here
-- Let's first look at the simplest case, where the string is entirely
-- filled with `?`s. In this case, there's a fairly simply formula we can use to
-- determine how many possible ways there are to fill it.
-- Say we have a set of groups to place, and a string.
-- It's relatively easy to see that the total number of .s that will be in the final
-- string will be numDots = length(string) - sum(groups)
--
-- How many different ways are there to fill the string with hotsprings? well we have numDots dots
-- and we can place each group either in between 2 dots or on the outside of the dots. For example
-- if we had 7 dots and 4 groups, we'd have to place the groups in the locations of 
-- 4 distinct |s as below
-- | . | . | . | . | . | . | . |
-- This is equal to (7+1) choose 4
-- So in general our formula for the all ?s case is (numDots + 1) choose numGroups

choose :: Int -> Int -> Int
choose n k 
    | n < 0 = 0
    | k > n = 0
    | otherwise = round $ product $ fmap ((\i -> (fromIntegral n + 1 - i) / i) . fromIntegral) [1..k]

-- So, from here, let's consider what happens if we add a single '.' to our
-- string. The dot splits the string into two sections around itself, and then
-- for every way to split our groups around the '.' multiply the possible ways to
-- put those groups on their respective sides

-- Now we just have to deal with #s. When we see a #, we split the groups we're
-- placing into the groups left of the #, the group that contains the #, and 
-- the groups to the right of the #, then we can multiply together the
-- possible arrangements of the left and the right for every placement of the
-- middle group over the #

countArrangements'' :: [Char] -> [Int] -> Int
countArrangements'' str counts = case break (\c -> c == '.' || c == '#') str of
    (_, []) -> (length str - sum counts + 1) `choose` length counts 
    (leadingQs, ('.':rest)) -> sum 
                             $ fmap (\(left, right) -> (length leadingQs - sum left + 1) `choose` length left
                                                     * memoCountArrangements rest right) 
                             $ filter (\(left, right) -> sum left + length left - 1 <= length leadingQs
                                                      && sum right + length right - 1 <= length rest)
                             $ zip (inits counts) (tails counts)
    (leadingQs, ('#':rest)) -> sum 
                             $ fmap (\(remainLeft, remainRight)-> uncurry memoCountArrangements remainLeft
                                                                * uncurry memoCountArrangements remainRight) 
                             $ concatMap (possiblePlacements leadingQs rest) 
                             $ splitInThree counts
  where
    splitInThree :: [Int] -> [([Int], Int, [Int])]
    splitInThree groups = zip3 (inits groups) groups (drop 1 $ tails groups)

-- memoize our function, dropping the runtime from ~28 seconds to ~5 seconds
memoCountArrangements :: [Char] -> [Int] -> Int
memoCountArrangements = memo2 countArrangements''

possiblePlacements :: [Char] -> [Char] -> ([Int], Int, [Int]) -> [(([Char], [Int]), ([Char], [Int]))]
possiblePlacements leftStr rightStr (leftCounts, groupToPlace, rightCounts) = 
    fmap (\index -> ((reverse $ drop (groupToPlace - index - 1 + 1) $ reverse leftStr,leftCounts)
                    ,(drop (index + 1) rightStr, rightCounts)))
    $ filter (canPlace leftStr rightStr leftCounts rightCounts groupToPlace) [0..(groupToPlace - 1)] 
    
canPlace :: [Char] -> [Char] -> [Int] -> [Int] -> Int -> Int -> Bool
canPlace leftStr rightStr leftCounts rightCounts groupToPlace index =
       sum leftCounts + length leftCounts - 1 <= length leftStr - (groupToPlace - index - 1) - 1 
    && sum rightCounts + length rightCounts - 1 <= length rightStr - index - 1
    && all (/= '.') (take index rightStr)
    && all (/= '#') (take 1 $ drop index rightStr)
    && all (/= '.') (take (groupToPlace - index - 1) leftStr)
    && all (/= '#') (take 1 $ drop (groupToPlace - index - 1) (reverse leftStr))

part2 :: [([Char], [Int])] -> Int
part2 = sum . fmap (uncurry memoCountArrangements . expandFiveTimes)
  where
    expandFiveTimes (str, counts) = (intercalate "?" $ replicate 5 str, concat $ replicate 5 counts)

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = take n xs : splitEvery n (drop n xs)

main :: IO ()
main = do
    input <- readInput
    putStrLn $ "Part 1 Solution: " ++ show (part1 input)
    putStrLn $ "Part 2 Solution: " ++ show (part2 input)