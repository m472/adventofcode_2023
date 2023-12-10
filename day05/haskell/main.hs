import Data.List (nub, sort)
import System.Posix (sleep)

splitOn :: (Eq a, Show a) => a -> [a] -> [[a]]
splitOn = splitOn' []

splitOn' :: (Eq a, Show a) => [a] -> a -> [a] -> [[a]]
splitOn' acc _ [] = [reverse acc]
splitOn' acc c (x : xs)
  | c == x = reverse acc : splitOn' [] c xs
  | otherwise = splitOn' (x : acc) c xs

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs [_] = error "uneven length in pairs"
pairs (a : b : rest) = (a, b) : pairs rest

headWithDefault :: a -> [a] -> a
headWithDefault def [] = def
headWithDefault _ (x : _) = x

inRange :: Int -> (Int, Int, Int) -> Bool
inRange n (_, srcStart, len) = n >= srcStart && n < srcStart + len

reverseInRange :: Int -> (Int, Int, Int) -> Bool
reverseInRange n (dstStart, _, len) = n >= dstStart && n < dstStart + len

convert :: Int -> [(Int, Int, Int)] -> Int
convert n ranges = n + (dstStart - srcStart)
  where
    (dstStart, srcStart, _) = headWithDefault (0, 0, 0) $ filter (inRange n) ranges

convertBack :: Int -> [(Int, Int, Int)] -> Int
convertBack n ranges = n - (dstStart - srcStart)
  where
    (dstStart, srcStart, _) = headWithDefault (0, 0, 0) $ filter (reverseInRange n) ranges

parseRange :: String -> (Int, Int, Int)
parseRange s = (srcStart, dstStart, len)
  where
    [srcStart, dstStart, len] = map read (words s)

parseInput :: String -> ([Int], [[(Int, Int, Int)]])
parseInput content = (seeds, ranges)
  where
    contentLines = lines content
    seeds = map read $ tail $ words $ head contentLines
    ranges = map (map parseRange . tail) $ splitOn "" $ drop 2 contentLines

computeDiscontinuities :: (Int, Int, Int) -> [Int]
computeDiscontinuities (_, srcStart, len) = [srcStart - 1, srcStart, srcStart + len, srcStart + len + 1]

partOne :: ([Int], [[(Int, Int, Int)]]) -> IO ()
partOne (seeds, ranges) = print $ minimum $ map (\seed -> foldl convert seed ranges) seeds

partTwo :: ([Int], [[(Int, Int, Int)]]) -> IO ()
partTwo (seeds, ranges) = do
  print $ minimum $ map (\seed -> foldl convert seed ranges) inputs
  where
    discontinuities = nub $ sort $ foldr (\r cum -> map (`convertBack` r) cum ++ concatMap computeDiscontinuities r) [] ranges
    seedRanges = pairs seeds
    inputs = sort $ concatMap (\(start, len) -> start : start + len : filter (\x -> x >= start && x < start + len) discontinuities) seedRanges

main = do
  content <- readFile "../input.txt"
  let input = parseInput content
  partOne input
  partTwo input
