{-# Language OverloadedStrings, LambdaCase #-}
module Main where
import Data.Char(ord, isAlpha)
import Data.Text(Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import Data.Attoparsec.Text(Parser)
import qualified  Data.Attoparsec.Text as A
import Data.IntMap.Strict(IntMap)
import qualified Data.IntMap.Strict as IM
import Data.Bits((.&.))
import Data.List(foldl', findIndex)
import Data.Either(fromRight)
import Control.Applicative((<|>))

readInput :: IO [Text]
readInput = fromRight (error "parse error") . A.parseOnly (A.takeWhile (/= ',') `A.sepBy` ",") <$> T.IO.readFile "input/day15.txt"

calcHash :: Text -> Int
calcHash = foldl' (\acc x -> ((acc + ord x) * 17) .&. 0xFF) 0 . T.unpack

part1 :: [Text] -> Int
part1 = sum . map calcHash

data Operation = Remove Text | Insert Text Int
    deriving (Show, Eq, Ord)

parseRemove :: Parser Operation
parseRemove = Remove <$> A.takeWhile1 isAlpha <* "-" <* A.endOfInput

parseInsert :: Parser Operation
parseInsert = Insert <$> A.takeWhile1 isAlpha <* "=" <*> A.decimal <* A.endOfInput

parseOperation :: Parser Operation
parseOperation = parseRemove <|> parseInsert

evalOperation :: IntMap [(Text, Int)] -> Operation -> IntMap [(Text, Int)]
evalOperation boxes (Remove label) = IM.adjust (filter ((/= label) . fst)) (calcHash label) boxes
evalOperation boxes (Insert label fl) = IM.alter insertIntoBox (calcHash label) boxes
  where
    insertIntoBox :: Maybe [(Text, Int)] -> Maybe [(Text, Int)]
    insertIntoBox Nothing = Just [(label, fl)]
    insertIntoBox (Just box) = case findIndex ((== label) . fst) box of
        Nothing -> Just $ (label, fl):box
        Just i -> Just $ take i box ++ (label, fl):(drop (i+1) box)

calcFocusingPower :: IntMap [(Text, Int)] -> Int
calcFocusingPower = sum 
    . concatMap (\(boxId, box) -> fmap (\(slotId, (_, focalLength)) -> (boxId + 1) * slotId * focalLength) 
                                $ zip [1..] (reverse box)) 
    . IM.toList

part2 :: [Text] -> Int
part2 = calcFocusingPower .  foldl' evalOperation IM.empty . fromRight (error "failed to parse ops") . traverse (A.parseOnly parseOperation)

main :: IO ()
main = do
    input <- readInput
    putStrLn $ "Part 1 Solution: " ++ show (part1 input)
    putStrLn $ "Part 2 Solution: " ++ show (part2 input)