import Data.Array (Array, Ix (inRange), array, bounds, elems, indices, (!), (//))
import Debug.Trace (trace, traceShowId)

data Direction = North | South | East | West deriving (Eq, Show)

parseInput :: String -> Array (Int, Int) Char
parseInput content =
  let inputMap = zipWith (\j line -> zipWith (\i c -> ((i, j), c)) [1 ..] line) [1 ..] (lines content)
   in array ((1, 1), (length inputMap, length (head inputMap))) (concat inputMap)

simulate :: Array (Int, Int) Char -> Array (Int, Int) Bool
simulate arr = simulate' [((1, 1), East)] [] (array (bounds arr) [(i, False) | i <- indices arr]) arr

simulate' :: [((Int, Int), Direction)] -> [((Int, Int), Direction)] -> Array (Int, Int) Bool -> Array (Int, Int) Char -> Array (Int, Int) Bool
simulate' [] _ res arr = res
simulate' ((pos, dir) : remainingNodes) visited res arr =
  let currentChar = arr ! pos
      newDir = changeDir dir currentChar
      newNodeList = filter (inRange (bounds arr) . fst) $ map (\d -> (move pos d, d)) newDir
   in simulate' (filter (`notElem` visited) (newNodeList ++ remainingNodes)) ((pos, dir) : visited) (res // [(pos, True)]) arr

changeDir :: Direction -> Char -> [Direction]
changeDir dir '.' = [dir]
changeDir East '-' = [East]
changeDir East '|' = [North, South]
changeDir East '/' = [North]
changeDir East '\\' = [South]
changeDir West '-' = [West]
changeDir West '|' = [North, South]
changeDir West '/' = [South]
changeDir West '\\' = [North]
changeDir North '|' = [North]
changeDir North '-' = [East, West]
changeDir North '/' = [East]
changeDir North '\\' = [West]
changeDir South '|' = [South]
changeDir South '-' = [East, West]
changeDir South '/' = [West]
changeDir South '\\' = [East]

move :: (Int, Int) -> Direction -> (Int, Int)
move (i, j) East = (i + 1, j)
move (i, j) West = (i - 1, j)
move (i, j) North = (i, j - 1)
move (i, j) South = (i, j + 1)

disp :: (Eq a) => (a -> Char) -> Array (Int, Int) a -> IO ()
disp f arr =
  let ((min_x, min_y), (max_x, max_y)) = bounds arr
   in putStrLn $ unlines [[f (arr ! (i, j)) | i <- [min_x .. max_x]] | j <- [min_y .. max_y]]

main = do
  content <- readFile "../input.txt"
  let input = parseInput content
  disp id input
  print $ length $ filter id $ elems $ simulate input
