import Data.List (intersect)

type Card = (Int, [Int], [Int])

splitOn :: (Eq a, Show a) => a -> [a] -> [[a]]
splitOn = splitOn' []

splitOn' :: (Eq a, Show a) => [a] -> a -> [a] -> [[a]]
splitOn' acc _ [] = [reverse acc]
splitOn' acc c (x : xs)
  | c == x = reverse acc : splitOn' [] c xs
  | otherwise = splitOn' (x : acc) c xs

parseCard :: String -> Card
parseCard line = (read (words card !! 1) :: Int, map read $ words winning, map read $ words actual)
  where
    [card, numbers] = splitOn ':' line
    [winning, actual] = splitOn '|' numbers

countWinningNumbers :: Card -> Int
countWinningNumbers (_, winning, actual) = length (actual `intersect` winning)

main = do
  content <- readFile "../input.txt"
  let cards = map parseCard $ lines content

  -- part One
  print $ sum $ map  ((`div` 2) . (2 ^ ) . countWinningNumbers) cards

  print $ map countWinningNumbers cards
