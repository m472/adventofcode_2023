import Debug.Trace

calcDist :: Integer -> Integer -> Integer
calcDist totalTime holdDownTime = (totalTime - holdDownTime) * holdDownTime

partOne :: String -> IO ()
partOne content = print $ product $ zipWith countWinningOptions distances times
  where
    [times, distances] = map (map read . tail . words) (lines content)
    countWinningOptions dist time = length $ filter (> dist) $ map (calcDist time) [1 .. time]

partTwo :: String -> IO ()
partTwo content = print $ length $ filter (> dist) $ map (calcDist time) [1 .. time]
  where
    [time, dist] = map (read . concat . tail . words) (lines content) :: [Integer]
    countWinningOptions dist time = length $ filter (> dist) $ map (calcDist time) [1 .. time]

main = do
  content <- readFile "../input.txt"
  partOne content
  partTwo content
