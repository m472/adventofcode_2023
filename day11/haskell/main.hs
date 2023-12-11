import Data.Array
import Data.List (nub, transpose)

expandLines :: [String] -> [String]
expandLines [] = []
expandLines (l : ls)
  | all (== '.') l = l : l : expandLines ls
  | otherwise = l : expandLines ls

parseInput :: String -> [(Int, Int)]
parseInput content =
  let imageLines = transpose $ expandLines $ transpose $ expandLines $ lines content
      image = zipWith (\i line -> zipWith (\j c -> ((i, j), c)) [1 ..] line) [1 ..] imageLines
   in map fst $ filter ((== '#') . snd) $ concat image

pairs :: (Ord a) => [a] -> [(a, a)]
pairs xs = [(a, b) | a <- xs, b <- xs, a < b]

dist :: (Int, Int) -> (Int, Int) -> Int
dist (ax, ay) (bx, by) = abs (bx - ax) + abs (by - ay)

main = do
  content <- readFile "../input.txt"
  let galaxyPositions = parseInput content
  let galaxyPairs = pairs galaxyPositions
  print $ sum $ map (uncurry dist) galaxyPairs
