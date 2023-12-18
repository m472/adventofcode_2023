import Data.Array (Array, array, elems, indices, (!), (//))
import Data.Char (ord)
import Data.List (elemIndex)
import Data.Maybe (fromJust)

type HASHMAP = Array Int [(String, Int)]

data Instruction = Remove String | AddOrUpdate String Int deriving (Show)

splitOn :: (Eq a, Show a) => a -> [a] -> [[a]]
splitOn = splitOn' []

splitOn' :: (Eq a, Show a) => [a] -> a -> [a] -> [[a]]
splitOn' acc _ [] = [reverse acc]
splitOn' acc c (x : xs)
  | c == x = reverse acc : splitOn' [] c xs
  | otherwise = splitOn' (x : acc) c xs

setAt :: Int -> a -> [a] -> [a]
setAt 0 val xs = val : tail xs
setAt ind val xs = take ind xs ++ [val] ++ drop (ind + 1) xs

hash :: [Char] -> Int
hash = foldl (\s c -> ((s + ord c) * 17) `mod` 256) 0

partOne :: [String] -> Int
partOne = sum . map hash

parseInstr :: String -> Instruction
parseInstr instr
  | last instr == '-' = Remove (init instr)
  | otherwise = AddOrUpdate label (read strength)
  where
    [label, strength] = splitOn '=' instr

process :: HASHMAP -> Instruction -> HASHMAP
process hm instr =
  let (boxIndex, newContent) = case instr of
        (Remove label) ->
          let ind = hash label
           in (ind, filter ((/= label) . fst) (hm ! ind))
        (AddOrUpdate label strength) ->
          let ind = hash label
              box = hm ! ind
           in case label `elemIndex` map fst box of
                Nothing -> (ind, box ++ [(label, strength)])
                (Just lensIndex) -> (ind, setAt lensIndex (label, strength) box)
   in hm // [(boxIndex, newContent)]

partTwo :: [String] -> Int
partTwo xs =
  let boxes = array (0, 255) [(i, []) | i <- [0 .. 255]] :: HASHMAP
      finalState = foldl process boxes $ map parseInstr xs :: HASHMAP
   in sum $
        map (\(i, e) -> i * sum (zipWith (*) [1 ..] $ map snd e)) $
          filter (not . null . snd) $
            zip [1 ..] (elems finalState)

main = do
  content <- readFile "../input.txt"
  let input = splitOn ',' $ head $ lines content

  print $ partOne input
  print $ partTwo input
