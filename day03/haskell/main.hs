import Data.Array
import Data.Bifunctor (first)
import Data.Char (isDigit)
import Data.List
import Debug.Trace

type Schema = Array (Int, Int) Char

parseInput :: String -> Array (Int, Int) Char
parseInput content =
  array ((0, 0), (rows - 1, cols - 1)) $
    concat $
      zipWith (\i line -> zipWith (\j c -> ((i, j), c)) [0 ..] line) [0 ..] (lines content)
  where
    rows = length $ lines content
    cols = length $ head $ lines content

neighborIndices :: Schema -> (Int, Int) -> [(Int, Int)]
neighborIndices arr (i, j) =
  let ((u_min, v_min), (u_max, v_max)) = bounds arr
   in filter
        (/= (i, j))
        [ (u, v)
          | u <- [(max (i - 1) u_min) .. (min (i + 1) u_max)],
            v <- [(max (j - 1) v_min) .. (min (j + 1) v_max)]
        ]

neighbors :: Schema -> (Int, Int) -> [Char]
neighbors arr (i, j) = map (arr !) (neighborIndices arr (i, j))

isSymbol :: Char -> Bool
isSymbol = (`elem` "*@=%+$&/-#")

splitNumbers = splitNumbers' []

splitNumbers' :: [((Int, Int), Char)] -> [((Int, Int), Char)] -> [[((Int, Int), Char)]]
splitNumbers' [] [] = []
splitNumbers' acc [] = [reverse acc]
splitNumbers' [] (c : cs)
  | isDigit (snd c) = splitNumbers' [c] cs
  | otherwise = splitNumbers' [] cs
splitNumbers' acc (c : cs)
  | isDigit (snd c) = splitNumbers' (c : acc) cs
  | otherwise = reverse acc : splitNumbers' [] cs

count :: (Eq a) => [a] -> [(a, Int)]
count = count' []

count' :: (Eq a) => [(a, Int)] -> [a] -> [(a, Int)]
count' acc [] = acc
count' acc (x : xs) = count' updatedAcc xs
  where
    updatedAcc = case lookup x acc of
      (Just cnt) -> (x, cnt + 1) : filter ((/= x) . fst) acc
      Nothing -> (x, 1) : acc

partOne :: Schema -> Int
partOne schema = sum partNumbers
  where
    partNumbers =
      map (read . map snd) $
        filter (any (any isSymbol . neighbors schema . fst)) $
          splitNumbers $
            zip (indices schema) (elems schema)

calculateGearRatios :: [((Int, Int), Int)] -> [((Int, Int), Int)]
calculateGearRatios = calculateGearRatios' []

calculateGearRatios' :: [((Int, Int), Int)] -> [((Int, Int), Int)] -> [((Int, Int), Int)]
calculateGearRatios' acc [] = acc
calculateGearRatios' acc ((ind, num) : xs) = calculateGearRatios' updatedAcc xs
  where
    updatedAcc = case lookup ind acc of
      (Just number) -> (ind, number * num) : filter ((/= ind) . fst) acc
      Nothing -> (ind, num) : acc

partTwo :: Schema -> Int
partTwo schema =
  let numbers = splitNumbers $ zip (indices schema) (elems schema)
      gearNumbers = map (nub . concatMap (filter ((== '*') . (schema !)) . neighborIndices schema . fst)) numbers
      gearPositions = map fst $ filter ((== 2) . snd) $ count $ concat gearNumbers
   in sum $
        map snd $
          calculateGearRatios $
            map (first head) $
              filter (any (`elem` gearPositions) . fst) $
                zip gearNumbers (map (read . map snd) numbers :: [Int])

main = do
  content <- readFile "../input.txt"
  let schema = parseInput content
  print $ partOne schema
