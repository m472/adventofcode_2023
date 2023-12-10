import Data.Array
import Data.List (nub, sort)
import Debug.Trace

data Direction = North | East | South | West deriving (Show, Eq, Ord)

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
    ((_, min_i), (_, max_i)) = bounds arr

subChar :: Char -> Char
subChar c = case c of
  '-' -> '─'
  '|' -> '│'
  'J' -> '┘'
  'L' -> '└'
  '7' -> '┐'
  'F' -> '┌'
  'S' -> 'S'
  '.' -> '·'
  'I' -> '■'
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

findStartInd :: Array (Int, Int) Char -> (Int, Int)
findStartInd arr =
  fst $
    head $
      filter ((== 'S') . snd) $
        zip (indices arr) (elems arr)

distances :: Array (Int, Int) Char -> [((Int, Int), Int)]
distances arr =
  smallestDist $
    concatMap
      (\dir -> distances' (dir, goto (findStartInd arr) dir) 1 arr)
      allDirections

distances' :: (Direction, (Int, Int)) -> Int -> Array (Int, Int) Char -> [((Int, Int), Int)]
distances' (dir, (i, j)) n arr =
  case navigate dir (arr ! (i, j)) of
    Just newDir ->
      ((i, j), n)
        : distances' (newDir, goto (i, j) newDir) (n + 1) arr
    Nothing -> []

dirFromCoords :: (Int, Int) -> (Int, Int) -> Direction
dirFromCoords (x, y) (u, v) = case (x - u, y - v) of
  (0, -1) -> East
  (0, 1) -> West
  (1, 0) -> North
  (-1, 0) -> South
  x -> error (show x)

extractMainLoop :: Array (Int, Int) Char -> [(Int, Int)]
extractMainLoop arr =
  start
    : head
      ( filter (/= []) $
          map
            (\dir -> extractMainLoop' (dir, goto start dir) 1 arr)
            allDirections
      )
  where
    start = findStartInd arr

extractMainLoop' :: (Direction, (Int, Int)) -> Int -> Array (Int, Int) Char -> [(Int, Int)]
extractMainLoop' (dir, (i, j)) n arr
  | inBounds (i, j) arr = case navigate dir (arr ! (i, j)) of
      Just newDir -> (i, j) : extractMainLoop' (newDir, goto (i, j) newDir) (n + 1) arr
      Nothing -> []
  | otherwise = []

inBounds :: (Int, Int) -> Array (Int, Int) a -> Bool
inBounds (i, j) arr =
  let ((min_i, min_j), (max_i, max_j)) = bounds arr
   in (i >= min_i && i <= max_i) && (j >= min_j && j <= max_j)

isInside :: [(Int, Int)] -> Array (Int, Int) Char -> (Int, Int) -> Bool
isInside mainLoop arr (i, j)
  | (i, j) `elem` mainLoop = False
  | otherwise =
      odd $
        countCrossings $
          filter (/= '-') $
            map (arr !) $
              sort $
                filter
                  (\(lineNr, colNr) -> lineNr == i && colNr < j)
                  mainLoop

convertStart :: (Int, Int) -> [(Int, Int)] -> Char
convertStart start loop =
  let begin = loop !! 1
      end = last loop
   in case sort [dirFromCoords start begin, dirFromCoords start end] of
        [North, South] -> '|'
        [North, East] -> 'L'
        [North, West] -> 'J'
        [East, West] -> '-'
        [East, South] -> 'F'
        [South, West] -> '7'

countCrossings :: [Char] -> Int
countCrossings [] = 0
countCrossings ('F' : 'J' : cs) = 1 + countCrossings cs
countCrossings ('L' : '7' : cs) = 1 + countCrossings cs
countCrossings ('F' : '7' : cs) = 2 + countCrossings cs
countCrossings ('L' : 'J' : cs) = 2 + countCrossings cs
countCrossings ('|' : cs) = 1 + countCrossings cs
countCrossings x = error ("non exhaustive: " ++ map subChar x)

partOne :: Array (Int, Int) Char -> IO ()
partOne pipeMap = do
  print $ maximum $ map snd $ distances pipeMap

partTwo :: Array (Int, Int) Char -> IO ()
partTwo pipeMap = do
  let mainLoop = extractMainLoop pipeMap
  let start = findStartInd pipeMap
  let pipeMapReplacedStart = pipeMap // [(start, convertStart start mainLoop)]

  let inside = filter (isInside mainLoop pipeMapReplacedStart) (indices pipeMap)
  prettyPrint pipeMapReplacedStart
  prettyPrint (pipeMap // map (,'I') inside)

  print $ length inside

main = do
  content <- readFile "../input.txt"
  let pipeMap = parseInput content
  prettyPrint pipeMap

  partOne pipeMap
  partTwo pipeMap
