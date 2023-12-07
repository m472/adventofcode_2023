import Data.List
import Data.Ord
import Debug.Trace

data HandType = HighCard | OnePair | TwoPairs | Three | FullHouse | Four | Five deriving (Show, Eq, Ord)

headWithDefault :: a -> [a] -> a
headWithDefault def [] = def
headWithDefault _ (x : _) = x

tailAllowEmpty :: [a] -> [a]
tailAllowEmpty [] = []
tailAllowEmpty (x : xs) = xs

runLengthEncode :: (Eq a) => [a] -> [(Int, a)]
runLengthEncode [] = []
runLengthEncode (x : xs) = runLengthEncode' (1, x) xs

runLengthEncode' :: (Eq a) => (Int, a) -> [a] -> [(Int, a)]
runLengthEncode' acc [] = [acc]
runLengthEncode' (count, char) (x : xs)
  | char == x = runLengthEncode' (count + 1, char) xs
  | otherwise = (count, char) : runLengthEncode' (1, x) xs

classifyCounts :: [Int] -> HandType
classifyCounts counts = case counts of 
  [1, 1, 1, 1, 1] -> HighCard
  [2, 1, 1, 1] -> OnePair
  [2, 2, 1] -> TwoPairs
  [3, 1, 1] -> Three
  [3, 2] -> FullHouse
  [4, 1] -> Four
  [5] -> Five
  x -> error ("could not classify " ++ show x)

classify :: String -> HandType
classify chars = classifyCounts $ sortBy (comparing Data.Ord.Down) (map fst $ runLengthEncode (sort chars))

classifyWithJoker :: String -> HandType
classifyWithJoker chars = classifyCounts countAfterJoker
    where
      counts = sortBy (comparing Data.Ord.Down) (runLengthEncode (sort chars))
      jokerCount = headWithDefault 0 $ map fst $ filter (('J' ==) . snd) counts
      countsWithoutJoker = map fst $ filter (('J' /=) . snd) counts
      countAfterJoker = headWithDefault 0 countsWithoutJoker + jokerCount : tailAllowEmpty countsWithoutJoker

compareHands :: Bool -> String -> String -> Ordering
compareHands withJoker a b
  | clf a > clf b = GT
  | clf a < clf b = LT
  | otherwise = compareHands' withJoker a b
  where
    clf = if withJoker then classifyWithJoker else classify

compareHands' :: Bool -> String -> String -> Ordering
compareHands' _ [] [] = EQ
compareHands' withJoker (a : as) (b : bs)
  | a == b = compareHands' withJoker as bs
  | otherwise = compare (cardValue withJoker a) (cardValue withJoker b)

cardValue :: Bool -> Char -> Int
cardValue _ 'A' = 14
cardValue _ 'K' = 13
cardValue _ 'Q' = 12
cardValue withJoker 'J' = if withJoker then 1 else 11
cardValue _ 'T' = 10
cardValue _ '9' = 9
cardValue _ '8' = 8
cardValue _ '7' = 7
cardValue _ '6' = 6
cardValue _ '5' = 5
cardValue _ '4' = 4
cardValue _ '3' = 3
cardValue _ '2' = 2
cardValue _ _ = error "unknown char"

partOne :: [String] -> [Int] -> IO ()
partOne hands bids = do
  print $ sum $ zipWith (*) [1 ..] $ map snd $ sortBy (\a b -> compareHands False (fst a) (fst b)) (zip hands bids)

partTwo :: [String] -> [Int] -> IO ()
partTwo hands bids = do
  --putStr $ unlines $ map (\(i, (hand, bid)) -> show (hand, classifyWithJoker hand, bid, i)) $ zip [1..] $ sortBy (\a b -> compareHands True (fst a) (fst b)) (zip hands bids)
  print $ sum $ zipWith (*) [1 ..] $ map snd $ sortBy (\a b -> compareHands True (fst a) (fst b)) (zip hands bids)

main = do
  content <- readFile "../input.txt"
  let hands = map (head . words) $ lines content
  let bids = map (read . last . words) $ lines content

  partOne hands bids
  partTwo hands bids
