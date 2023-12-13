module Main where
import qualified Data.Text.IO as T.IO
import Data.Attoparsec.Text
import Data.Set (Set)
import qualified Data.Set as S
import Control.Applicative((<|>))
import Data.Maybe(listToMaybe, fromMaybe)
import Data.Either(fromRight)

gridToIndices :: [[Char]] -> ((Int, Int), Set (Int, Int))
gridToIndices grid = 
    let numRows = length grid
        numColumns = length $ fromMaybe [] $ listToMaybe grid
        pointIndices = S.fromList $ fmap fst $ concatMap (\(y, row) -> filter ((== '#') . snd) $ zip (zip (repeat y) [0..]) row) $ zip [0..] grid
    in ((numRows, numColumns), pointIndices)

parseGrid :: Parser ((Int, Int), Set (Int, Int))
parseGrid = gridToIndices <$> many1 (char '.' <|> char '#') `sepBy` endOfLine

parseInput :: Parser [((Int, Int), Set (Int, Int))]
parseInput = parseGrid `sepBy` (count 2 endOfLine)

readInput :: IO [((Int, Int), Set (Int, Int))]
readInput = fromRight (error "parse error") . parseOnly parseInput <$> T.IO.readFile "input/day13.txt"

findVerticalMirror :: ((Int, Int), Set (Int, Int)) -> [Int]
findVerticalMirror ((_, rowLen), points) = 
    let mirroredPoints :: Set (Int, Int) -> Int -> Set (Int, Int)
        mirroredPoints s flipCol = S.filter (\(_, x) -> x >= 0)
                                 $ S.map (\(y, x) -> (y,2 * flipCol + 1 - x)) 
                                 $ S.filter (\(_, x) -> x > flipCol) s
        isReflection :: Set (Int, Int) -> (Int, Set (Int, Int)) -> Bool
        isReflection s1 (flipCol, s2) = S.filter (\(_, x) -> x <= flipCol && x > rowLen + 1 - 2 * (rowLen - flipCol)) s1 == s2
    in fmap ((+1) . fst) $ filter (isReflection points) $ zip [0..] $ fmap (mirroredPoints points) [0..rowLen - 2]

findHorizontalMirror :: ((Int, Int), Set (Int, Int)) -> [Int]
findHorizontalMirror ((colHeight, rowLen), points) = findVerticalMirror ((rowLen, colHeight), S.map (\(y, x) -> (x, y)) points)

findGridVal :: ((Int, Int), Set (Int, Int)) -> [Int]
findGridVal gridInfo = findVerticalMirror gridInfo ++ (fmap (*100) $ findHorizontalMirror gridInfo)

part1 :: [((Int, Int), Set (Int, Int))] -> Int
part1 = sum . concatMap findGridVal

flipCell :: (Int, Int) -> Set (Int, Int) -> Set (Int, Int)
flipCell c m = if c `S.member` m then S.delete c m else S.insert c m

-- Is this slow? yes very but it works
testAllSmudges :: ((Int, Int), Set (Int, Int)) -> Maybe Int
testAllSmudges ((colHeight, rowLen), points) =
    let originalGridVal = fromMaybe (error "no original grid val?") $ listToMaybe $ findGridVal ((colHeight, rowLen), points) 
    in listToMaybe 
     $ fmap (\k -> if k `mod` 100 /= 0 && k `div` 100 /= 0 
                    then abs (k - originalGridVal)
                    else k)
     $ filter (\v -> v /= originalGridVal && v /= 0)
     $ concatMap (\(y, x) -> findGridVal ((colHeight, rowLen), flipCell (y, x) points)) 
     $ liftA2 (,) [0..colHeight - 1] [0..rowLen - 1]

part2 :: [((Int, Int), Set (Int, Int))] -> Maybe Int
part2 = fmap sum . traverse testAllSmudges

main :: IO ()
main = do
    input <- readInput
    putStrLn $ "Part 1 Solution: " ++ show (part1 input)
    putStrLn $ "Part 2 Solution: " ++ show (part2 input)