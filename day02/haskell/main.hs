import Data.Char
import Data.List
import Data.Map (Map (), adjust, findWithDefault, fromList, toList)
import Debug.Trace

type Round = [(Int, String)]
type Game = (Int, [Round])

splitOn :: (Eq a, Show a) => a -> [a] -> [[a]]
splitOn = splitOn' []

splitOn' :: (Eq a, Show a) => [a] -> a -> [a] -> [[a]]
splitOn' acc _ [] = [reverse acc]
splitOn' acc c (x : xs)
  | c == x = reverse acc : splitOn' [] c xs
  | otherwise = splitOn' (x : acc) c xs

parseLiteral :: String -> String -> String
parseLiteral [] [] = []
parseLiteral _ [] = error "unexpected end of input"
parseLiteral [] s = s
parseLiteral (w : ws) (x : xs) = if w == x then parseLiteral ws xs else error "expected '" ++ [w] ++ "' but found '" ++ [x] ++ "' instead"

parseNumber :: String -> (Int, String)
parseNumber s = case parseNumber' [] s of
  ([], remainder) -> error ("No number found at: " ++ remainder)
  (digits, remainder) -> (read digits :: Int, remainder)

parseNumber' :: String -> String -> (String, String)
parseNumber' acc (x : xs)
  | isDigit x = parseNumber' (x : acc) xs
  | otherwise = (reverse acc, x : xs)

rstrip :: String -> String
rstrip = dropWhile (== ' ')

parseRocks :: String -> (Int, String)
parseRocks s = (read count, color)
  where
    [count, color] = words s

parseGame :: String -> Game
parseGame s = (gameId, map (map parseRocks . splitOn ',') $ splitOn ';' (parseLiteral ": " remainder))
  where
    (gameId, remainder) = parseNumber $ parseLiteral "Game " s

minRocks :: [Round] -> Map String Int
minRocks game = foldr (\(cnt, color) -> adjust (max cnt) color) colorCounts (concat game)
  where
    colorCounts = fromList $ map (,0) (concatMap (map snd) game)

unique :: (Eq a) => [a] -> [a]
unique = unique' []

unique' :: (Eq a) => [a] -> [a] -> [a]
unique' acc [] = reverse acc
unique' acc (x : xs)
  | x `elem` acc = unique' acc xs
  | otherwise = unique' (x : acc) xs

isPossible :: [(String, Int)] -> [Round] -> Bool
isPossible rockCounts game = foldr (\(color, cnt) reduced -> reduced && (cnt >= findWithDefault 0 color minRockCounts)) True rockCounts
  where
    minRockCounts = minRocks game

partOne :: [Game] -> IO()
partOne games = do
  let givenRocks = [("red", 12), ("green", 13), ("blue", 14)]
  print $ sum $ map fst $ filter (isPossible givenRocks . snd) games

partTwo :: [Game] -> IO()
partTwo games = do
  print $ sum $ map (product . map snd . toList . minRocks . snd) games

main = do
  content <- readFile "../input.txt"
  let games = map parseGame $ lines content
  partOne games
  partTwo games
