type SpringRecord = ([Char], [Int])

splitOn :: (Eq a, Show a) => a -> [a] -> [[a]]
splitOn = splitOn' []

splitOn' :: (Eq a, Show a) => [a] -> a -> [a] -> [[a]]
splitOn' acc _ [] = [reverse acc]
splitOn' acc c (x : xs)
  | c == x = reverse acc : splitOn' [] c xs
  | otherwise = splitOn' (x : acc) c xs

runLengthEncode :: (Eq a) => [a] -> [(Int, a)]
runLengthEncode [] = []
runLengthEncode (x : xs) = runLengthEncode' (1, x) xs

runLengthEncode' :: (Eq a) => (Int, a) -> [a] -> [(Int, a)]
runLengthEncode' acc [] = [acc]
runLengthEncode' (count, char) (x : xs)
  | char == x = runLengthEncode' (count + 1, char) xs
  | otherwise = (count, char) : runLengthEncode' (1, x) xs

runLengthDecode :: [(Int, a)] -> [a]
runLengthDecode = concatMap (uncurry replicate)

to2Tuple :: [a] -> (a, a)
to2Tuple [a, b] = (a, b)
to2Tuple _ = error "list must have exactly two elements"

parseInput :: String -> [SpringRecord]
parseInput content =
  let parts = map (to2Tuple . words) $ lines content
      springMap = map fst parts
      counts = map (map read . splitOn ',' . snd) parts :: [[Int]]
   in zip springMap counts

isKnown :: Char -> Bool
isKnown = (/= '?')

prime :: SpringRecord -> SpringRecord
prime ('.' : xs, cnt) = prime (xs, cnt)
prime ('#' : xs, 1 : cnts) = prime (xs, cnts)
prime ('#' : xs, c : cnts) = prime (xs, c - 1 : cnts)
prime x = x

rev :: SpringRecord -> SpringRecord
rev (chars, counts) = (reverse chars, reverse counts)

generateOptions :: [Char] -> [[Char]]
generateOptions ['?'] = [['#'], ['.']]
generateOptions [c] = [[c]]
generateOptions ('?' : xs) = [opt : rest | opt <- ['#', '.'], rest <- generateOptions xs]
generateOptions (c : xs) = map (c :) (generateOptions xs)

isValid :: [Int] -> [Char] -> Bool
isValid counts chars = map fst (filter ((== '#') . snd) $ runLengthEncode chars) == counts

main = do
  content <- readFile "../input.txt"
  print $ sum $ map (\(springs, cnts) -> length $ filter (isValid cnts) $ generateOptions springs) $ parseInput content
  -- print $ map (rev . prime . rev . prime) $ parseInput content
