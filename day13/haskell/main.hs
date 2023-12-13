import Data.List
import Debug.Trace (traceShowId, trace)

splitOn :: (Eq a, Show a) => a -> [a] -> [[a]]
splitOn = splitOn' []

splitOn' :: (Eq a, Show a) => [a] -> a -> [a] -> [[a]]
splitOn' acc _ [] = [reverse acc]
splitOn' acc c (x : xs)
  | c == x = reverse acc : splitOn' [] c xs
  | otherwise = splitOn' (x : acc) c xs

mirrorAt :: Int -> [a] -> ([a], [a])
mirrorAt i xs = (reverse a, b)
  where
    (a, b) = splitAt i xs

isExactlyNDifferences :: (Eq a, Show a) => Int -> ([[a]], [[a]]) -> Bool
isExactlyNDifferences 0 ([], []) = True
isExactlyNDifferences 0 ([], _) = True
isExactlyNDifferences 0 (_, []) = True
isExactlyNDifferences _ ([], _) = False
isExactlyNDifferences _ (_, []) = False
isExactlyNDifferences n (a : as, b : bs) = (differenceCount <= n) && isExactlyNDifferences (n - differenceCount) (as, bs)
  where
    differenceCount = length $ filter id $ zipWith (/=) a b

findMirrorIndex :: [[Char]] -> Maybe Int
findMirrorIndex xs =
  let firstEqual = head $ filter (isExactlyNDifferences 0) [mirrorAt i xs | i <- [1 ..]]
   in case firstEqual of
        (_, []) -> Nothing
        (a, _) -> Just (length a)

findSmudgyMirrorIndex :: [[Char]] -> Maybe Int
findSmudgyMirrorIndex xs =
  let exactlyOneDiff = filter (isExactlyNDifferences 1) [mirrorAt i xs | i <- [1 .. length xs]]
   in case exactlyOneDiff of
        [] -> Nothing
        [(_, [])] -> Nothing
        [(a, _)] -> Just (length a)
        _ -> error "did not expect more than one"

mirrorNumber :: [[Char]] -> Int
mirrorNumber xs = case findMirrorIndex xs of
  Just row -> 100 * row
  Nothing -> case findMirrorIndex (transpose xs) of
    Just col -> col
    Nothing -> error "there should be exactly one mirror, found none"

mirrorNumberSmudge :: [[Char]] -> Int
mirrorNumberSmudge xs = case findSmudgyMirrorIndex xs of
  Just row -> 100 * row
  Nothing -> case findSmudgyMirrorIndex (transpose xs) of
    Just col -> col
    Nothing -> error "there should be exactly one smudgy mirror, found none"

main = do
  content <- readFile "../input.txt"
  print $ sum $ map mirrorNumber $ splitOn [] $ lines content
  print $ sum $ map mirrorNumberSmudge $ splitOn [] $ lines content
