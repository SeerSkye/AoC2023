module Main where
import Data.Map.Strict(Map)
import qualified Data.Map.Strict as M
import Data.Set(Set)
import qualified Data.Set as S
import Data.Maybe(listToMaybe, fromMaybe, mapMaybe)
import Control.Monad(forM_)

-- Too lazy to do proper array stuff for this, so just gonna
-- use a map with coordinate keys instead.
readInput :: IO (Map (Int, Int) Char)
readInput = makeMap . lines <$> readFile "input/day10.txt"

makeMap :: [[Char]] -> Map (Int, Int) Char
makeMap ls = M.fromList $ concatMap (\(y, row) -> zip (zip (repeat y) [0..]) row) $ zip [0..] ls

data Dir = North | South | East | West deriving (Show, Eq)

-- Given a pipe, and the direction moved to reach that pipe
-- return the direction needed to continue along that pipe, if
-- possible
nextDir :: Char -> Dir -> Maybe Dir
nextDir '|' North = Just North
nextDir '|' South = Just South
nextDir '-' East  = Just East
nextDir '-' West  = Just West
nextDir 'L' South = Just East
nextDir 'L' West  = Just North
nextDir 'J' South = Just West
nextDir 'J' East  = Just North
nextDir '7' North = Just West
nextDir '7' East  = Just South
nextDir 'F' North = Just East
nextDir 'F' West  = Just South
nextDir _   _     = Nothing

move :: (Int, Int) -> Dir -> (Int, Int)
move (y, x) North = (y-1, x)
move (y, x) South = (y+1, x)
move (y, x) East  = (y, x+1)
move (y, x) West  = (y, x-1)

findLoop :: Map (Int, Int) Char -> Maybe (Map (Int, Int) Dir)
findLoop m = 
    let start = fst 
              $ fromMaybe (error "No starting location in map??")
              $ listToMaybe 
              $ filter ((== 'S') . snd) 
              $ M.toList m
        initialSearchNodes = [ (North, move start North)
                             , (South, move start South)
                             , (East,  move start East)
                             , (West,  move start West)]
    in listToMaybe $ mapMaybe (\(dir, loc) -> findLoop' m (M.singleton start dir) dir loc) initialSearchNodes

findLoop' :: Map(Int, Int) Char -> Map (Int, Int) Dir -> Dir -> (Int, Int) -> Maybe (Map (Int, Int) Dir)
findLoop' m seen dir loc
    | loc `M.member` seen = Just seen
    | otherwise = do
        currTile <- M.lookup loc m
        nextDirection <- nextDir currTile dir
        let nextLoc = move loc nextDirection
        findLoop' m (M.insert loc dir seen) nextDirection nextLoc

part1 :: Map (Int, Int) Char -> Maybe Int
part1 = fmap ((`div` 2) . M.size) . findLoop  

-- Pretty printer I made to help visualize the path
printPath :: Map (Int, Int) Char -> Map (Int, Int) Dir -> IO ()
printPath m path = 
    let (maxY, maxX) = fromMaybe (0,0) $ fst <$> M.lookupMax m
        arrowify :: Char -> Dir -> Char
        arrowify '|' North = '↑' 
        arrowify '|' South = '↓'
        arrowify '-' East = '→'
        arrowify '-' West = '←'
        arrowify 'L' South = '└'
        arrowify 'L' West = '└'
        arrowify 'J' South = '┘'
        arrowify 'J' East = '┘'
        arrowify '7' North = '┐'
        arrowify '7' East = '┐'
        arrowify 'F' North = '┌'
        arrowify 'F' West = '┌'
        arrowify 'S' _    = 'S'
        arrowify _   _    = '?'
        pathMap = M.intersectionWith arrowify m path
    in forM_ [0..maxY] $ \y ->
        forM_ [0..maxX] (\x -> putStr $ pure (M.findWithDefault '.' (y, x) pathMap)) >> putStrLn ""
        
-- So, to answer this question we have to come up with a more useful definition of 
-- "inside the loop". The loop divides space into 2 sections: every space to its right is
-- in one and every space to its left is in the other. "Outside" is whichever one of those divisions
-- happens to contain the edge of the map.

-- Given the map and a path, create lists of all the points next to the path to the left and right
splitSpace :: Map (Int, Int) Char -> Map (Int, Int) Dir -> (Set (Int, Int), Set (Int, Int))
splitSpace m path =
    let (lefts, rights) = foldr mergeSets (S.empty, S.empty) $ fmap splitAround (M.toList path)
    in (floodFill m path lefts, floodFill m path rights)
  where 
    splitAround :: ((Int, Int), Dir) -> (Set (Int, Int), Set (Int, Int))
    splitAround (loc, dir) = 
        let (lefts, rights) = adjacentPoints (M.findWithDefault '?' loc m) dir loc
        in ( S.fromList (filter (\l -> l `M.notMember` path && l `M.member` m) lefts)
           , S.fromList (filter (\r -> r `M.notMember` path && r `M.member` m) rights))
    mergeSets :: (Set (Int, Int), Set (Int, Int)) 
              -> (Set (Int, Int), Set (Int, Int)) 
              -> (Set (Int, Int), Set (Int, Int))
    mergeSets (newLefts, newRights) (lefts, rights) = ( newLefts `S.union` lefts
                                                      , newRights `S.union` rights)

-- Given a pipe segment, a direction, and a location, return the adjacent
-- locations that could be directly inside or outside the loop
adjacentPoints :: Char -> Dir -> (Int, Int) -> ([(Int, Int)], [(Int, Int)])
adjacentPoints '|' North loc = ([move loc West], [move loc East])
adjacentPoints '|' South loc = ([move loc East], [move loc West])
adjacentPoints '-' East  loc = ([move loc North], [move loc South])
adjacentPoints '-' West  loc = ([move loc South], [move loc North])
adjacentPoints 'L' South loc = ([move (move loc North) East]
                               ,[move loc West, move loc South, move (move loc South) West])
adjacentPoints 'L' West  loc = ([move loc West, move loc South, move (move loc South) West]
                               ,[move (move loc North) East])
adjacentPoints 'J' South loc = ([move loc East, move loc South, move (move loc South) East]
                               ,[move (move loc North) West])
adjacentPoints 'J' East  loc = ([move (move loc North) West]
                               ,[move loc East, move loc South, move (move loc South) East])
adjacentPoints '7' North loc = ([move (move loc South) West]
                               ,[move loc East, move loc North, move (move loc North) East])
adjacentPoints '7' East  loc = ([move loc East, move loc North, move (move loc North) East]
                               ,[move (move loc South) West])
adjacentPoints 'F' North loc = ([move loc West, move loc North, move (move loc North) West]
                               ,[move (move loc South) East])
adjacentPoints 'F' West  loc = ([move (move loc South) East]
                               ,[move loc West, move loc North, move (move loc North) West])
-- ignoring the starting point is fine, as any points inside/outside will in connected areas
-- touching at least 2 sections of the path
adjacentPoints 'S' _     _   = ([],[])
adjacentPoints _   _     _   = error "adjacentPoints given values that don't correspond to a path"

-- Take a set of points either inside or outside the path, and flood fill to get all the 
-- points connected to those starting points, without crossing the path
floodFill :: Map (Int, Int) Char -> Map (Int, Int) Dir -> Set (Int, Int) -> Set (Int, Int)
floodFill m path startingPoints = go (S.toList startingPoints) S.empty
  where
    nextPoints :: (Int, Int) -> [(Int, Int)]
    nextPoints loc = [move loc North, move loc South, move loc East, move loc West]
    go :: [(Int, Int)] -> Set (Int, Int) -> Set (Int, Int)
    go [] seen = seen
    go (p:ps) seen 
        -- The point is a point on the path, so it's not inside or outside
        | p `M.member` path = go ps seen
        -- Restrict our floodfill to only spaces inside the bounds of the given map,
        -- we don't want to be floodfilling forever
        | p `M.notMember` m = go ps seen
        -- If we've already seen a point, move on
        | p `S.member` seen = go ps seen
        -- Otherwise add all 4 directions from our point to the stack, and add the point to
        -- our list of points visited
        | otherwise = go (nextPoints p ++ ps) (S.insert p seen)

part2 :: Map (Int, Int) Char -> Maybe Int
part2 m = do
    path <- findLoop m
    let (left, right) = splitSpace m path
    if any (\(y, x) -> y == 0 || x == 0) left then pure (S.size right) else pure (S.size left)

main :: IO ()
main = do
    input <- readInput
    putStrLn $ "Part 1 Solution: " ++ show (part1 input)
    putStrLn $ "Part 2 Solution: " ++ show (part2 input)