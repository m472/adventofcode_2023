import Data.List
import Debug.Trace (traceShowId)

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

findMirrorIndex :: [[Char]] -> Maybe Int
findMirrorIndex xs =
  let firstEqual = head $ filter (all (uncurry (==)) . uncurry zip) [mirrorAt i xs | i <- [1 ..]]
   in case firstEqual of
        (_, []) -> Nothing
        (a, _) -> Just (length a)

mirrorNumber :: [[Char]] -> Int
mirrorNumber xs = case findMirrorIndex xs of
  Just row -> 100 * row
  Nothing -> case findMirrorIndex (transpose xs) of
    Just col -> col
    Nothing -> error "there should be exactly one mirror"

main = do
  content <- readFile "../input.txt"
  print $ sum $ map mirrorNumber $ splitOn [] $ lines content
