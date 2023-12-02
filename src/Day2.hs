{-# LANGUAGE OverloadedStrings #-}
module Main where
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import Data.Attoparsec.Text
import Data.Maybe(fromMaybe)
import Data.List (find)
import Data.Either(fromRight)
import Control.Applicative((<|>))

cube :: Parser (T.Text, Int)
cube = flip (,) <$> decimal <* space <*> ("red" <|> "green" <|> "blue") 

data Draw = Draw
    { red   :: Int 
    , blue  :: Int
    , green :: Int
    } deriving (Show, Eq)

drawFromAssoc :: [(T.Text, Int)] -> Draw
drawFromAssoc lst = Draw { red = r, blue = b, green = g}
  where
    getColorAmount s = fromMaybe 0 $ snd <$> find ((== s) . fst) lst
    r = getColorAmount "red"
    b = getColorAmount "blue"
    g = getColorAmount "green"

draw :: Parser Draw
draw = drawFromAssoc <$> cube `sepBy` ", "

data Game = Game
    { getID  :: Int
    , rounds :: [Draw]
    } deriving (Show, Eq)

game :: Parser Game
game = Game 
    <$> ("Game " *> decimal <* ": ")
    <*> draw `sepBy` "; " <* (endOfLine <|> endOfInput)

readInput :: IO [Game]
readInput = fromRight (error "parse error") . parseOnly (many' game) <$> T.IO.readFile "input/day2.txt"

isValidRound :: Draw -- The maximum possible draw
             -> Draw -- The draw to be checked
             -> Bool
isValidRound Draw{red = maxred, green = maxgreen, blue = maxblue}
             Draw{red = drawnred, green = drawngreen, blue = drawnblue} = 
    drawnred <= maxred && drawngreen <= maxgreen && drawnblue <= maxblue

isValidGame :: Draw -> Game -> Bool
isValidGame limits = and . fmap (isValidRound limits) . rounds

part1 :: [Game] -> Int
part1 = sum . fmap getID . filter (isValidGame (Draw {red = 12, green = 13, blue = 14})) 

-- Find the minimum number of cubes for a game to be possible
findLimits :: Game -> Draw
findLimits Game{rounds = rs} =
    foldr f Draw{red = 0, green = 0, blue = 0} rs
  where
    f Draw{red = r, green = g, blue = b} 
      Draw{red = limitr, green = limitg, blue = limitb} = 
        Draw{red = max r limitr, green = max g limitg, blue = max b limitb}

powerFromLimits :: Draw -> Int
powerFromLimits Draw {red = r, green = g, blue = b} = r*g*b

part2 :: [Game] -> Int
part2 = sum . fmap (powerFromLimits . findLimits)

main :: IO ()
main = do
    input <- readInput
    putStrLn $ "Part 1 Solution: " ++ show (part1 input)
    putStrLn $ "Part 2 Solution: " ++ show (part2 input)