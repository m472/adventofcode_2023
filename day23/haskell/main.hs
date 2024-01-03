import Data.Array (Array, array, bounds, (!))
import Data.List (minimumBy, sortBy, sortOn)
import Data.Ord (Down (Down), comparing)
import Debug.Trace (traceShowId)

type Position = (Int, Int)
type Pose = (Position, Direction)
type HikingMap = Array Position Char
data Direction = North | South | West | East deriving (Eq, Show)

parseInput :: String -> HikingMap
parseInput content =
    let inputMap =
            zipWith
                (\j line -> zipWith (\i c -> ((i, j), c)) [1 ..] line)
                [1 ..]
                (lines content)
     in array ((1, 1), (length (head inputMap), length inputMap)) (concat inputMap)

disp :: (a -> Char) -> Array Position a -> [Char]
disp f arr =
    let ((min_x, min_y), (max_x, max_y)) = bounds arr
     in unlines
            [ [ f (arr ! (i, j))
              | i <- [min_x .. max_x]
              ]
            | j <- [min_y .. max_y]
            ]

direction :: Position -> Position -> Direction
direction (x, y) (u, v)
    | x == u && y == v + 1 = North
    | x == u && y == v - 1 = South
    | x == u - 1 && y == v = East
    | x == u + 1 && y == v = West
    | otherwise = error "invalid direction"

add :: Position -> Position -> Position
add (x, y) (u, v) = (x + u, y + v)

invertDir :: Direction -> Direction
invertDir North = South
invertDir South = North
invertDir West = East
invertDir East = West

neighborIndices :: Array Position a -> Position -> [Position]
neighborIndices arr (i, j) =
    let ((u_min, v_min), (u_max, v_max)) = bounds arr
     in filter
            (\(u, v) -> (u >= u_min && u <= u_max) && (v >= v_min && v <= v_max))
            [ (i - 1, j)
            , (i + 1, j)
            , (i, j - 1)
            , (i, j + 1)
            ]

checkDir :: Direction -> Char -> Bool
checkDir North '^' = True
checkDir South 'v' = True
checkDir East '>' = True
checkDir West '<' = True
checkDir _ '.' = True
checkDir _ _ = False

options :: Pose -> HikingMap -> [Pose]
options (pos, dir) hikingMap =
    filter (\(pos, dir) -> let c = (hikingMap ! pos) in (c /= '#') && checkDir dir c)
        $ filter ((/= invertDir dir) . snd)
        $ map (\p -> (p, direction pos p))
        $ neighborIndices hikingMap pos

findPath :: HikingMap -> Position -> Pose -> [Pose]
findPath hikingMap target start =
    start : case map (findPath hikingMap target) $ options start hikingMap of
        [] -> []
        subPaths ->
            minimumBy
                (comparing (Data.Ord.Down . length))
                subPaths

main = do
    content <- readFile "../input.txt"
    let hikingMap = parseInput content
    let start = add (1, 0) $ fst $ bounds hikingMap
    let target = add (-1, 0) $ snd $ bounds hikingMap
    putStrLn $ disp id hikingMap
    print $ (+ (-1)) $ length $ findPath hikingMap target (start, South)
