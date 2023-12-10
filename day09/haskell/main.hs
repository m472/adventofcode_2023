import Data.List

diff :: [Int] -> [Int]
diff [] = []
diff [_] = []
diff (a : b : xs) = b - a : diff (b : xs)

repeatedDiff :: [Int] -> [[Int]]
repeatedDiff xs
  | all (== 0) xs = [xs]
  | otherwise = xs : repeatedDiff (diff xs)

extrapolate :: [Int] -> Int
extrapolate = sum . (map last . repeatedDiff)

extrapolateBack :: [Int] -> Int
extrapolateBack = foldr ((-) . head) 0 . repeatedDiff

partOne :: [[Int]] -> Int
partOne = sum . map extrapolate

partTwo :: [[Int]] -> Int
partTwo = sum . map extrapolateBack

main = do
  content <- readFile "../input.txt"
  let timeSeries = map (map read . words) $ lines content :: [[Int]]
  print (partOne timeSeries)
  print (partTwo timeSeries)
