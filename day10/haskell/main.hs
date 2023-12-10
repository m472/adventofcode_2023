import Data.Array
import Debug.Trace

data Direction = North | East | South | West deriving (Show)

split :: Int -> [a] -> [[a]]
split n [] = []
split n xs = take n xs : split n (drop n xs)

parseInput :: String -> Array (Int, Int) Char
parseInput content =
  let pipes = zipWith (\i line -> zipWith (\j c -> ((i, j), c)) [1 ..] line) [1 ..] (lines content)
   in array ((1, 1), (length pipes, length (head pipes))) (concat pipes)

prettyPrint :: Array (Int, Int) Char -> IO ()
prettyPrint arr = putStr $ unlines $ split (max_i - min_i + 1) $ map subChar (elems arr)
  where
    ((min_i, _), (max_i, _)) = bounds arr
    subChar c = case c of
      '-' -> '─'
      '|' -> '│'
      'J' -> '┘'
      'L' -> '└'
      '7' -> '┐'
      'F' -> '┌'
      'S' -> 'S'
      '.' -> ' '
      c -> error ("unexpected char: " ++ [c])

navigate :: Direction -> Char -> Maybe Direction
navigate dir c = case (dir, c) of
  -- North
  (North, '|') -> Just North
  (North, 'F') -> Just East
  (North, '7') -> Just West
  (North, _) -> Nothing
  -- South
  (South, '|') -> Just South
  (South, 'J') -> Just West
  (South, 'L') -> Just East
  (South, _) -> Nothing
  -- West
  (West, '-') -> Just West
  (West, 'F') -> Just South
  (West, 'L') -> Just North
  (West, _) -> Nothing
  -- East
  (East, '-') -> Just East
  (East, 'J') -> Just North
  (East, '7') -> Just South
  (East, _) -> Nothing

goto :: (Int, Int) -> Direction -> (Int, Int)
goto ind dir = case (ind, dir) of
  ((i, j), North) -> (i - 1, j)
  ((i, j), South) -> (i + 1, j)
  ((i, j), East) -> (i, j + 1)
  ((i, j), West) -> (i, j - 1)

allDirections :: [Direction]
allDirections = [North, East, South, West]

smallestDist :: [((Int, Int), Int)] -> [((Int, Int), Int)]
smallestDist = foldl f []
  where
    f res (ind, dist) = case lookup ind res of
      (Just currentDist) -> (ind, min dist currentDist) : filter ((/= ind) . fst) res
      Nothing -> (ind, dist) : res

distances :: Array (Int, Int) Char -> [((Int, Int), Int)]
distances arr = smallestDist $ concatMap (\dir -> distances' (dir, goto start dir) 1 arr) allDirections
  where
    start =
      fst $
        head $
          filter ((== 'S') . snd) $
            zip (indices arr) (elems arr)

distances' :: (Direction, (Int, Int)) -> Int -> Array (Int, Int) Char -> [((Int, Int), Int)]
distances' (dir, (i, j)) n arr =
  case navigate dir (arr ! (i, j)) of
    Just newDir -> ((i, j), n) : distances' (newDir, goto (i, j) newDir) (n + 1) arr
    Nothing -> []

partOne :: Array (Int, Int) Char -> IO ()
partOne pipeMap = do
  print $ maximum $ map snd $ distances pipeMap

main = do
  content <- readFile "../input.txt"
  let pipeMap = parseInput content
  prettyPrint pipeMap
  
  partOne pipeMap
