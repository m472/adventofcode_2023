import Data.Map (Map, assocs, elems, empty, fromList, insert, (!), (!?))
import Data.Maybe (fromJust, isJust)
import Debug.Trace (trace, traceShowId)

data Module = FlipFlop State [String] | Conjunction (Map String Pulse) [String] | Broadcaster [String] deriving (Show)

data Pulse = Low | High deriving (Show, Eq, Ord)

data State = On | Off deriving (Show, Eq)

splitOn :: (Eq a) => [a] -> [a] -> ([a], [a])
splitOn pattern str = case first pattern str of
  Nothing -> (str, [])
  Just index ->
    let l = length pattern
        (first, second) = splitAt (index - l) str
     in (first, drop l second)

first :: (Eq a) => [a] -> [a] -> Maybe Int
first pattern = first' 0 pattern pattern

first' :: (Eq a) => Int -> [a] -> [a] -> [a] -> Maybe Int
first' n _ _ [] = Nothing
first' n _ [] _ = Just n
first' n pattern (a : as) (b : bs)
  | a == b = first' (n + 1) pattern as bs
  | (a : as) == pattern = first' (n + 1) pattern pattern bs
  | otherwise = first' n pattern pattern (b : bs)

parseModules :: String -> Map String Module
parseModules content = fromList $ map parseModule $ lines content

parseModule :: String -> (String, Module)
parseModule s =
  let (sender, recipientStr) = splitOn " -> " s
      recipients = map (filter (/= ',')) $ words recipientStr
   in parseModule' sender recipients

parseModule' :: String -> [String] -> (String, Module)
parseModule' "broadcaster" recipients = ("broadcaster", Broadcaster recipients)
parseModule' ('%' : name) recipients = (name, FlipFlop Off recipients)
parseModule' ('&' : name) recipients = (name, Conjunction empty recipients)

getTargets :: Module -> [String]
getTargets (FlipFlop _ targets) = targets
getTargets (Conjunction _ targets) = targets
getTargets (Broadcaster targets) = targets

initModules :: Map String Module -> Map String Module
initModules modules = fromList $ map (initModule modules) (assocs modules)

initModule :: Map String Module -> (String, Module) -> (String, Module)
initModule modules (moduleName, Conjunction _ recipients) = (moduleName, Conjunction (fromList [(name, Low) | name <- connectedModules]) recipients)
  where
    connectedModules = map fst $ filter ((moduleName `elem`) . getTargets . snd) $ assocs modules :: [String]
initModule _ x = x

simulate :: [(String, Pulse, String)] -> (Map Pulse Int, Map String Module) -> (Map Pulse Int, Map String Module)
simulate [] state = state
simulate ((sender, pulse, receiver) : remainingPulses) (pulseCounts, state) =
  let newPulseCounts = insert pulse (pulseCounts ! pulse + 1) pulseCounts
   in case state !? receiver of
        Nothing -> simulate remainingPulses (newPulseCounts, state)
        Just mod ->
          let (newModuleState, newPulse) = simulate' sender pulse mod
           in simulate
                (remainingPulses ++ [(receiver, fromJust newPulse, r) | isJust newPulse, r <- getTargets mod])
                (newPulseCounts, insert receiver newModuleState state)

simulate' :: String -> Pulse -> Module -> (Module, Maybe Pulse)
simulate' _ High (FlipFlop state recipients) = (FlipFlop state recipients, Nothing)
simulate' _ Low (FlipFlop state recipients) = (FlipFlop (flipState state) recipients, Just (pulse state))
  where
    flipState state = case state of
      Off -> On
      On -> Off
    pulse oldState = case oldState of
      Off -> High
      On -> Low
simulate' sender pulse (Conjunction memory recipients) =
  let updatedMemory = insert sender pulse memory
   in ( Conjunction updatedMemory recipients,
        Just (if all (== High) (elems updatedMemory) then Low else High)
      )
simulate' _ Low (Broadcaster name) = (Broadcaster name, Just Low)
simulate' _ _ (Broadcaster _) = error "did not expect that"

main = do
  content <- readFile "../input.txt"
  let modules = parseModules content
  print $
    product $
      fst
        ( iterate
            (simulate [("button", Low, "broadcaster")])
            (fromList [(Low, 0), (High, 0)], initModules modules)
            !! 1000
        )
