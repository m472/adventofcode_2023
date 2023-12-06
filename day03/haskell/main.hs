import Data.Array

parseInput :: String -> Array (Int, Int) Char
parseInput content = array ((0, 0), (rows - 1, cols - 1)) $ concat $ zipWith (\i line -> zipWith (\j c -> ((i, j), c)) [0 ..] line) [0 ..] (lines content)
  where
    rows = length $ lines content
    cols = length $ head $ lines content

main = do
  content <- readFile "../example.txt"
  let schema = parseInput content
  filter isDigit schema
