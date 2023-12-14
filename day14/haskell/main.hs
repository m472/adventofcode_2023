import Data.List (elemIndex, findIndex, transpose)
import Data.Maybe (fromJust)
import Debug.Trace

data Direction = North | South | East | West deriving (Show, Eq)

roll :: [Char] -> [Char]
roll = roll' []

roll' :: [Char] -> [Char] -> [Char]
roll' acc [] = reverse acc
roll' acc ('.' : 'O' : xs) = roll (reverse acc ++ 'O' : '.' : xs)
roll' acc (x : xs) = roll' (x : acc) xs

rollTo :: Direction -> [[Char]] -> [[Char]]
rollTo North = transpose . map roll . transpose
rollTo East = map (reverse . roll . reverse)
rollTo South = transpose . map (reverse . roll . reverse) . transpose
rollTo West = map roll

rollCycle :: [[Char]] -> [[Char]]
rollCycle xs = foldl (flip rollTo) xs [North, West, South, East]

simulateCycles :: Int -> [[Char]] -> Int
simulateCycles n = simulateCycles' n []

simulateCycles' :: Int -> [[[Char]]] -> [[Char]] -> Int
simulateCycles' 0 _ xs = calculateLoad xs
simulateCycles' n acc xs =
  let afterCycle = rollCycle xs
      newLoad = calculateLoad afterCycle
   in case afterCycle `elemIndex` reverse acc of
        Nothing -> simulateCycles' (n - 1) (afterCycle : acc) afterCycle
        Just startOfFirstCycle ->
          let endOfFirstCycle = length acc
           in calculateLoad $ reverse acc !! (startOfFirstCycle + ((n - 1) `mod` (endOfFirstCycle - startOfFirstCycle)))

calculateLoad :: [[Char]] -> Int
calculateLoad xs = sum $ map (sum . map fst . filter ((== 'O') . snd) . zip [1 ..] . reverse) (transpose xs)

partOne :: String -> IO ()
partOne content = print $ calculateLoad $ rollTo North $ lines content

partTwo :: String -> IO ()
partTwo content = print $ simulateCycles 1_000_000_000 $ lines content

main :: IO ()
main = do
  content <- readFile "../input.txt"
  partOne content
  partTwo content
