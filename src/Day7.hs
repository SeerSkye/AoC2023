module Main where
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import qualified Data.Map as M
import Data.List(sortBy)
import Data.Function(on)
import Control.Arrow((&&&))
import Data.Maybe(fromMaybe, listToMaybe)

data Card = Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King
    | Ace 
    deriving (Show, Eq, Ord)

data HandType = HighCard
    | OnePair
    | TwoPair
    | ThreeOfAKind
    | FullHouse
    | FourOfAKind
    | FiveOfAKind 
    deriving (Show, Eq, Ord)

charToCard :: Char -> Maybe Card
charToCard '2' = Just Two
charToCard '3' = Just Three
charToCard '4' = Just Four
charToCard '5' = Just Five
charToCard '6' = Just Six
charToCard '7' = Just Seven
charToCard '8' = Just Eight
charToCard '9' = Just Nine
charToCard 'T' = Just Ten
charToCard 'J' = Just Jack
charToCard 'Q' = Just Queen
charToCard 'K' = Just King
charToCard 'A' = Just Ace
charToCard _ = Nothing

makeHand :: T.Text -> [Card]
makeHand = fromMaybe [] . traverse charToCard . T.unpack

scoreHand :: [Card] -> Maybe HandType
scoreHand cards = 
    let cardCountMap = foldr (\card counts -> M.insertWith (+) card (1 :: Int) counts) M.empty cards
        cardCounts = sortBy (flip compare) $ snd <$> M.toList cardCountMap
    in case cardCounts of
        [5]             -> Just FiveOfAKind
        [4, 1]          -> Just FourOfAKind
        [3, 2]          -> Just FullHouse
        [3, 1, 1]       -> Just ThreeOfAKind
        [2, 2, 1]       -> Just TwoPair
        [2, 1, 1, 1]    -> Just OnePair
        [1, 1, 1, 1, 1] -> Just HighCard
        _               -> Nothing 

readInput :: IO [([Card], Int)]
readInput = fmap ((makeHand . T.take 5) &&& (read . T.unpack . T.drop 6)) . T.lines 
    <$> T.IO.readFile "input/day7.txt"

part1 :: [([Card], Int)] -> Maybe Int
part1 hands = 
    let addHandType :: ([Card], Int) -> Maybe ((HandType, [Card]), Int)
        addHandType (hand, bid) = case scoreHand hand of
            Nothing -> Nothing
            Just ty -> Just ((ty, hand), bid)
    in sum . zipWith (*) [1..] . fmap snd . sortBy(compare `on` fst) <$> traverse addHandType hands

-- Use a wrapper with a new Ord instance for the new card ordering
newtype P2Card = P2Card Card deriving (Show, Eq)
instance Ord P2Card where
    compare :: P2Card -> P2Card -> Ordering
    compare (P2Card Jack) (P2Card Jack) = EQ
    compare (P2Card Jack) (P2Card _)    = LT
    compare (P2Card _)    (P2Card Jack) = GT
    compare (P2Card c1)   (P2Card c2)   = compare c1 c2

-- I'm fairly certain that adding the Js to whatever non-J card
-- the hand has the most of produces the best hand? You want to add
-- A single J to a pair to make three of a kind over two pair, and
-- you'd want to add two Js to a pair to make 4 of a kind over a
-- full house
scoreHandp2 :: [Card] -> Maybe HandType
scoreHandp2 cards = 
    let cardCountMap = foldr (\card counts -> M.insertWith (+) card (1 :: Int) counts) M.empty cards
        cardCounts = sortBy (flip compare `on` snd) $ M.toList cardCountMap
        mostCommonNonJ = fromMaybe Jack $ listToMaybe $ filter (/= Jack) $ fmap fst cardCounts
        modifiedHand = fmap (\card -> if card == Jack then mostCommonNonJ else card) cards
    in scoreHand modifiedHand

-- Pretty much the same as P1, but wrapping the hands in the P2Card wrapper type
-- and using our new scoring function
part2 :: [([Card], Int)] -> Maybe Int
part2 hands = 
    let addHandType :: ([Card], Int) -> Maybe ((HandType, [P2Card]), Int)
        addHandType (hand, bid) = case scoreHandp2 hand of
            Nothing -> Nothing
            Just ty -> Just ((ty, fmap P2Card hand), bid)
    in sum . zipWith (*) [1..] . fmap snd . sortBy(compare `on` fst) <$> traverse addHandType hands

main :: IO ()
main = do
    input <- readInput
    putStrLn $ "Part 1 Solution: " ++ show (part1 input)
    putStrLn $ "Part 2 Solution: " ++ show (part2 input)