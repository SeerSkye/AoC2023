module Main where
import Data.List(transpose)

readInput :: IO [[Char]]
readInput = lines <$> readFile "input/day11.txt"

dupeEmpty :: [[Char]] -> [[Char]]
dupeEmpty = concatMap (\row -> if all (== '.') row then [row, row] else [row])

expandUniverse :: [[Char]] -> [[Char]]
expandUniverse = dupeEmpty . transpose . dupeEmpty . transpose

findGalaxyCoords :: [[Char]] -> [(Int, Int)]
findGalaxyCoords universe = fmap fst $ filter ((== '#') . snd) $ concatMap (\(y, row) -> zip (zip (repeat y) [0..]) row) $ zip [0..] universe

manhattanDistance :: (Int, Int) -> (Int, Int) -> Int
manhattanDistance (y1, x1) (y2, x2) = abs (y1 - y2) + abs (x1 - x2)

findAllDistances :: [(Int, Int)] -> [Int]
findAllDistances [] = []
findAllDistances (p:ps) = fmap (manhattanDistance p) ps ++ findAllDistances ps

part1 :: [[Char]] -> Int
part1 = sum . findAllDistances . findGalaxyCoords . expandUniverse

-- Aww part 2 was "now do it properly instead of just duplicating the rows"
findEmptyRows :: [[Char]] -> [Int]
findEmptyRows universe = fmap fst $ filter (all (== '.') . snd) $ zip [0..] universe

-- an expansionFactor of 2 means that we add 1 extra row, hence the -1 in the formulas
expandUniverse' :: Int -> [Int] -> [Int] -> [(Int, Int)] -> [(Int, Int)]
expandUniverse' expansionFactor emptyRows emptyColumns =
    fmap (\(y, x) -> (y + (expansionFactor - 1) * length (takeWhile (< y) emptyRows)
                     ,x + (expansionFactor - 1) * length (takeWhile (< x) emptyColumns)))

part2 :: [[Char]] -> Int
part2 universe = sum $ findAllDistances $ expandUniverse' 1000000 emptyRows emptyCols galaxyCoords
  where
    emptyRows = (findEmptyRows universe)
    emptyCols = (findEmptyRows (transpose universe)) 
    galaxyCoords = (findGalaxyCoords universe)

main :: IO ()
main = do
    input <- readInput
    putStrLn $ "Part 1 Solution: " ++ show (part1 input)
    putStrLn $ "Part 2 Solution: " ++ show (part2 input)