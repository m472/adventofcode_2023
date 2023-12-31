import Data.Char
import Data.Maybe (fromJust, isJust)
import Debug.Trace

type Map = [(String, (String, String))]

toTuple :: [a] -> (a, a)
toTuple [a, b] = (a, b)

parseInput :: String -> (String, Map)
parseInput content = (instructions, zip (map head nodes) (map (toTuple . tail) nodes))
  where
    (instructions : _ : nodes_str) = lines content
    nodes = map (filter (/= []) . map (filter isAlphaNum) . words) nodes_str

simulate :: [Char] -> Map -> Int
simulate instructions = simulate' 0 "AAA" (cycle instructions)

simulate' :: Int -> String -> [Char] -> Map -> Int
simulate' n current (i : is) map = if current == "ZZZ" then n else simulate' (n + 1) (findNextNode i map current) is map

findNextNode :: Char -> Map -> String -> String
findNextNode instr ghostMap current = case (lookup current ghostMap, instr) of
  (Just (left, _), 'L') -> left
  (Just (_, right), 'R') -> right
  (Nothing, _) -> error ("error looking up key " ++ current)

simulateAll :: [Char] -> Map -> Int
simulateAll instructions ghostMap =
  let startNodes = filter ((== 'A') . last) $ map fst ghostMap
   in simulateAll' 0 startNodes (replicate (length startNodes) Nothing) (cycle instructions) ghostMap

simulateAll' :: Int -> [String] -> [Maybe Int] -> [Char] -> Map -> Int
simulateAll' n current cycles (i : is) ghostMap
  | all ((== 'Z') . last) current = n
  | all isJust cycles = foldr (lcm . fromJust) 1 cycles
  | otherwise = simulateAll' (n + 1) (map (findNextNode i ghostMap) current) updatedCycles is ghostMap
  where
    updatedCycles = zipWith (update n) cycles current

update :: Int -> Maybe Int -> String -> Maybe Int
update n cycle current
  | last current == 'Z' = case cycle of
      Nothing -> Just n
      c -> c
  | otherwise = cycle

main = do
  content <- readFile "../input.txt"
  let (instructions, ghostMap) = parseInput content
  print $ simulate instructions ghostMap
  print $ simulateAll instructions ghostMap
