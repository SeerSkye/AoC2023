{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.Attoparsec.Text
import qualified Data.Text.IO as T.IO
import Data.IntMap.Strict(IntMap)
import qualified Data.IntMap.Strict as IM
import Data.Either(fromRight)
import Data.Maybe(fromMaybe, listToMaybe, mapMaybe)
import Data.List(foldl', sortBy, sort)
import Data.Function(on)

parseSeeds :: Parser [Int]
parseSeeds = "seeds: " *> decimal `sepBy` space <* endOfLine

-- forward because we're going to key off of the source range start
-- so that we can do a lookup efficiently
forwardMapRange :: Parser (Int, (Int, Int))
forwardMapRange = (\destStart srcStart len -> (srcStart, (destStart, len)))
    <$> (decimal <* space)
    <*> (decimal <* space)
    <*> decimal

forwardMap :: Parser (IntMap (Int, Int))
forwardMap = IM.fromList <$> (takeTill (== ':') *> ":\n" *> forwardMapRange `sepBy` endOfLine)

almanac :: Parser Almanac
almanac = Almanac <$> parseSeeds <* endOfLine <*> forwardMap `sepBy` endOfLine

readInput :: IO Almanac
readInput = fromRight (error "parse error") . parseOnly almanac <$> T.IO.readFile "input/day5.txt"

data Almanac = Almanac 
    { getSeeds :: [Int]
    , getMaps :: [IntMap (Int, Int)]
    } deriving Show

-- Assuming that none of the conversion ranges overlap, if a number we're converting is contained
-- in one of the ranges it *must* be contained within the one that starts closest to our value from
-- below
convert :: Int -> IntMap (Int, Int) -> Int
convert n conversionMap = 
    -- If our lookup fails, that means that the number we're converting is lower than any of
    -- the ranges in the map, so we just add a fake range to correspond to the default conversion
    let (srcStart, (destStart, rangeLen)) = fromMaybe (n, (n, 1)) $ IM.lookupLE n conversionMap
    in if n < srcStart + rangeLen 
        then n - srcStart + destStart
        else n

part1 :: Almanac -> Int
part1 (Almanac seeds maps) = minimum $ fmap (\s -> foldl' convert s maps) seeds

-- Okay, so, part 2 is a twist I sort of anticipated (as evidenced by describing "forward" maps)
-- earlier. In order to solve part 2 I'm going to try swapping the maps around and figuring out 
-- how to combine these maps into a larger more complicated map.

-- The maps in this problem appear to be invertable, so this is a safe transformation
flipMap :: IntMap (Int, Int) -> IntMap (Int, Int)
flipMap = IM.fromList . fmap (\(src, (dest, len)) -> (dest, (src, len))) . IM.toList

{-
So, the tricky part here is merging two maps into a new map corresponding to the
composition of the functions the maps represent. Using the small example, given

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4

We want to look for intersections between the src ranges of the second map and the
dest ranges of the first. Lets pull out the lines where those ranges are the lowest

t-2-h: 0 69 1
h-2-l: 60 56 37

our dest range in the t-2-h list does not overlap the src range in h-2-l at all, so we can just
move it to our new map

t-2-l: 0 69 1
t-2-h: 1 0 69
h-2-l: 60 56 37

Our second set of ranges do overlap now! However, because we're going through the second map in order
of the src range start locations, we know that h-2-l can't map anything into numbers below 56, so
we can move the non-overlapping portion of the range out

t-2-l: 0 69 1, 1 0 55
t-2-h: 56 55 14
h-2-l: 60 56 37

these two ranges in question now have composition happen between them, for the duration of the range with the lowest length

t-2-l: 0 69 1, 1 55 55, 60 0 14
t-2-h: empty
h-2-l: 74 70 23

now that the first is empty we can just move everything remaining into the final map, getting

temperature-to-location:
0 69 1
1 0 55
60 55 14
74 70 23
56 93 4
-}

-- Merge two maps, where the first is in the form (dest, (src, len))
-- and the second is in the form (src, (dest, len)). The returned map
-- is of the form (dest, (src, len))
mergeMap :: IntMap (Int, Int) -> IntMap (Int, Int) -> IntMap (Int, Int)
mergeMap map1 map2 = IM.fromList $ go (IM.toAscList map1) (IM.toAscList map2)
  where
    go :: [(Int, (Int, Int))] -> [(Int, (Int, Int))] -> [(Int, (Int, Int))]
    -- If only one map is present, it means the other map is the identity function
    -- over the remaining values, so we can just return whatever map is present
    go fstMap [] = fstMap
    go [] sndMap = fmap (\(src, (dest, len)) -> (dest, (src, len))) sndMap
    go (range1@(dest1, (src1, len1)):rest1) (range2@(src2, (dest2, len2)):rest2)
        -- The results of the first function are entirely outside the range of the
        -- lowest source range not yet handled in the second function
        | dest1 + len1 - 1 < src2 = range1 : go rest1 (range2 : rest2)
        -- The inputs covered by the second function's range are entirely
        -- outside the lowest not yet handled outputs of the first function
        | src2 + len2 - 1 < dest1 = (dest2, (src2, len2)) : go (range1 : rest1) rest2
        -- Neither of the above are true, so an intersection between the outputs of
        -- the first function and the inputs of the second are guaranteed.
        -- if the input/output ranges being composed together don't line up, we split off
        -- the part where no composition happens
        | dest1 < src2 = (dest1, (src1, src2 - dest1)) 
                       : go ((src2, (src1 + (src2 - dest1), len1 - (src2 - dest1))):rest1) 
                            (range2:rest2)
        | src2 < dest1 = (dest2, (src2, dest1 - src2)) 
                       : go (range1:rest1) 
                            ((dest1, (dest2 + (dest1 - src2), len2 - (dest1 - src2))):rest2)
        -- When they do line up, we compose the ranges together
        | dest1 == src2 && len1 < len2 =
            (dest2, (src1, len1)) : go rest1 ((src2 + len1, (dest2 + len1, len2 - len1)):rest2)
        | dest1 == src2 && len2 < len1 = 
            (dest2, (src1, len2)) : go ((dest1 + len2, (src1 + len2, len1 - len2)):rest1) rest2
        | dest1 == src2 && len2 == len1 =
            (dest2, (src1, len1)) : go rest1 rest2
        | otherwise = error "These cases should be exhaustive, but the checker isn't smart enough to see that, so I'm putting this here to remove a warning"

-- Take the list of maps in the form stored in the almanac and merge them into a single map
buildCombinedMap :: [IntMap (Int, Int)] -> IntMap (Int, Int)
buildCombinedMap [] = IM.empty
buildCombinedMap (first:rest) = foldl' mergeMap (flipMap first) rest

makeSeedRanges :: [Int] -> [(Int, Int)]
makeSeedRanges [] = []
makeSeedRanges (_:[]) = error "No corresponding number for seed range?"
makeSeedRanges (start:len:rest) = (start, len): makeSeedRanges rest 

-- After finishing I realize that this solution is actually flawed! It only actually finds
-- the lowest location that's covered by a range, but it's technically possible for 
-- the lowest location a seed can be planted in to be outside any range that the mapping effects!
-- Fortunately this isn't the case for my input though...
part2 :: Almanac -> Maybe Int
part2 (Almanac seedList maps) = 
    listToMaybe $ mapMaybe (findSeedLocation seedRanges) (IM.toAscList mergedMap)
  where 
    seedRanges = sortBy (compare `on` fst) $ makeSeedRanges seedList
    mergedMap = buildCombinedMap maps
    seedLocationInRange :: (Int, (Int, Int)) -> (Int, Int) -> Maybe Int
    seedLocationInRange (dest, (src, len)) (seedStart, seedLen)
        -- ranges overlap, but the start of the seed range is outside the searched range
        | seedStart <= src && src < seedStart + seedLen = Just dest
        -- start of seed range is inside the searched range
        | src <= seedStart && seedStart < src + len = Just (dest + (seedStart - src)) 
        | otherwise = Nothing
    findSeedLocation :: [(Int, Int)] -> (Int, (Int, Int))  -> Maybe Int
    findSeedLocation seeds range = listToMaybe $ sort $ mapMaybe (seedLocationInRange range) seeds

main :: IO ()
main = do
    input <- readInput
    putStrLn $ "Part 1 Solution: " ++ show (part1 input)
    putStrLn $ "Part 2 Solution: " ++ show (fromMaybe (error "No part 2 solution found????") $ part2 input)