{-# Language OverloadedStrings #-}
module Main where
import Data.Text(Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import Data.Map.Strict(Map)
import qualified Data.Map.Strict as M
import Data.Attoparsec.Text(Parser)
import qualified Data.Attoparsec.Text as A
import Data.IntMap.Strict(IntMap)
import qualified Data.IntMap.Strict as IM
import Control.Applicative((<|>))
import Data.Either(fromRight)
import Data.Maybe(listToMaybe, fromMaybe)

type Graph = Map Text (Text, Text)

data Dir = L | R deriving (Show, Eq)

parseL :: Parser Dir
parseL = L <$ A.char 'L'

parseR :: Parser Dir
parseR = R <$ A.char 'R'

parseDir :: Parser Dir
parseDir = parseR <|> parseL

parseNode :: Parser (Text, (Text, Text))
parseNode = (\key left right -> (key, (left, right)))
    <$> (A.take 3 <* " = (")
    <*> (A.take 3 <* ", ")
    <*> (A.take 3 <* ")")

parseInput :: Parser ([Dir], Graph)
parseInput = (,)
    <$> (A.many1 parseDir <* A.endOfLine <* A.endOfLine)
    <*> (M.fromList <$> parseNode `A.sepBy` A.endOfLine)

readInput :: IO ([Dir], Graph)
readInput = fromRight (error "parse error") . A.parseOnly parseInput <$> T.IO.readFile "input/day8.txt"

moveDir :: Graph -> Dir -> Text -> Maybe Text
moveDir graph L node = fst <$> M.lookup node graph
moveDir graph R node = snd <$> M.lookup node graph

part1 :: ([Dir], Graph) -> Int
part1 (dirs, graph) = part1' (cycle dirs) graph "AAA" 0
   
part1' :: [Dir] -> Graph -> Text -> Int -> Int
part1' [] _ _ _ = error "ran out of directions???"
part1' (d:ds) g currNode stepsTaken 
    | currNode == "ZZZ" = stepsTaken
    | otherwise = case (moveDir g d currNode) of
        Just nextNode -> part1' ds g nextNode (stepsTaken + 1)
        Nothing -> error $ "Couldn't move" ++ show d ++ " from " ++ show currNode

-- Fortunately the input is designed specifically so that the first time each starting node
-- reaches an ending node is *exactly* the same as the length of the cycle each path ends up
-- in, which means that that ending nodes will be reached on exact multiples of the answers
-- to a modified version of part 1. As such we can take the lcm of those cycle lengths
-- instead of having to do the full on chinese remainder theorem thing.
part2' :: [Dir] -> Graph -> Text -> Int -> Int
part2' [] _ _ _ = error "ran out of directions???"
part2' (d:ds) g currNode stepsTaken 
    | T.index currNode 2 == 'Z' = stepsTaken
    | otherwise = case (moveDir g d currNode) of
        Just nextNode -> part2' ds g nextNode (stepsTaken + 1)
        Nothing -> error $ "Couldn't move" ++ show d ++ " from " ++ show currNode

part2 :: ([Dir], Graph) -> Int
part2 (dirs, graph) = foldr lcm 1 $ fmap (\node -> part2' (cycle dirs) graph node 0) startingNodes
  where
    startingNodes = filter (\node -> T.index node 2 == 'A') $ M.keys graph

main :: IO ()
main = do
    input <- readInput
    putStrLn $ "Part 1 Solution: " ++ show (part1 input)
    putStrLn $ "Part 2 Solution: " ++ show (part2 input)