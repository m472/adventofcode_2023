import Data.Map (Map, assocs, elems, fromList, insert, update, (!))
import Data.Tuple (swap)
import Debug.Trace (trace, traceShowId)

data Result = Accept | Reject | WorkflowReference String deriving (Eq, Show)

data Rule = Unconditional Result | Conditional [Char] Int (Part -> Int) (Int -> Int -> Bool) Result

data Part = Part {x :: Int, m :: Int, a :: Int, s :: Int} deriving (Show)

data RuleTree = RuleNode {attr :: Char, oper :: Char, threshold :: Int, applies :: RuleTree, doesntApply :: RuleTree} | Leaf Result deriving (Show)

type AttributeRanges = Map Char (Int, Int)

type Workflows = Map String [Rule]

instance Show Rule where
  show (Unconditional result) = "Unconditional " ++ show result
  show (Conditional [attr, opr] threshold getter oper result) = "Conditional " ++ [attr] ++ " " ++ [opr] ++ " " ++ show threshold ++ " -> " ++ show result

splitOn :: (Eq a, Show a) => a -> [a] -> [[a]]
splitOn = splitOn' []

splitOn' :: (Eq a, Show a) => [a] -> a -> [a] -> [[a]]
splitOn' acc _ [] = [reverse acc]
splitOn' acc c (x : xs)
  | c == x = reverse acc : splitOn' [] c xs
  | otherwise = splitOn' (x : acc) c xs

parseResult :: String -> Result
parseResult "A" = Accept
parseResult "R" = Reject
parseResult s = WorkflowReference s

parseCondition :: String -> ([Char], Int, Part -> Int, Int -> Int -> Bool)
parseCondition (attr : opr : amount) =
  let getAttr = case attr of
        'x' -> x
        'm' -> m
        'a' -> a
        's' -> s
        _ -> error "unexpected attr"
      fun = case opr of
        '>' -> (>)
        '<' -> (<)
        _ -> error "unexpected opr"
   in -- in fun (read amount) . getAttr
      ([attr, opr], read amount, getAttr, fun)

parseRule :: String -> Rule
parseRule s = case splitOn ':' s of
  [result] -> Unconditional (parseResult result)
  [cond, result] ->
    let (desc, thr, getAttr, opr) = parseCondition cond
     in Conditional desc thr getAttr opr (parseResult result)
  _ -> error "unexpected"

parseWorkflow :: String -> (String, [Rule])
parseWorkflow s =
  let name = takeWhile (/= '{') s
      rules = map parseRule $ splitOn ',' $ init $ drop (length name + 1) s
   in (name, rules)

parsePart :: String -> Part
parsePart s =
  let [xs, ms, as, ss] = map (read . last . splitOn '=') $ splitOn ',' $ init $ tail s :: [Int]
   in Part xs ms as ss

parseInput :: String -> (Workflows, [Part])
parseInput s =
  let [workflows, parts] = splitOn [] $ lines s
   in (fromList (map parseWorkflow workflows), map parsePart parts)

applyRules :: Part -> [Rule] -> Result
applyRules _ ((Unconditional result) : rs) = result
applyRules part ((Conditional _ thr getter opr result) : rs)
  | getter part `opr` thr = result
  | otherwise = applyRules part rs

classify :: Workflows -> Part -> Result
classify = classify' "in"

classify' :: String -> Workflows -> Part -> Result
classify' name workflows part =
  let rules = workflows ! name
   in case applyRules part rules of
        Accept -> Accept
        Reject -> Reject
        WorkflowReference nextName -> classify' nextName workflows part

createTree :: Workflows -> RuleTree
createTree workflows = createTree' (workflows ! "in") workflows

createTree' :: [Rule] -> Workflows -> RuleTree
createTree' (r : rules) workflows =
  case r of
    Unconditional result -> toNode result workflows
    Conditional [attr, opr] threshold _ _ result -> RuleNode attr opr threshold (toNode result workflows) (createTree' rules workflows)
  where
    toNode result workflows = case result of
      Accept -> Leaf Accept
      Reject -> Leaf Reject
      WorkflowReference name -> createTree' (workflows ! name) workflows

partOne :: Workflows -> [Part] -> Int
partOne workflows parts = sum $ map partSum $ filter (\p -> classify workflows p == Accept) parts
  where
    partSum p = x p + m p + a p + s p

updatePartRange :: AttributeRanges -> Char -> Char -> Int -> (Maybe AttributeRanges, Maybe AttributeRanges)
updatePartRange ranges attr '<' threshold
  | threshold >= lower && threshold <= upper = (Just (insert attr (lower, last) ranges), Just (insert attr (first, upper) ranges))
  | threshold > upper = (Just ranges, Nothing)
  | threshold < lower = (Nothing, Just ranges)
  where
    (lower, upper) = ranges ! attr
    (last, first) = (threshold - 1, threshold)
updatePartRange ranges attr '>' threshold
  | threshold >= lower && threshold <= upper = (Just (insert attr (first, upper) ranges), Just (insert attr (lower, last) ranges))
  | threshold > upper = (Nothing, Just ranges)
  | threshold < lower = (Just ranges, Nothing)
  where
    (lower, upper) = ranges ! attr
    (last, first) = (threshold, threshold + 1)

countPossibilities :: AttributeRanges -> RuleTree -> Int
countPossibilities p (Leaf Accept) = product $ map ((+ 1) . uncurry (-) . swap) $ elems p
countPossibilities p (Leaf Reject) = 0
countPossibilities p (RuleNode {attr = attr, threshold = threshold, oper = oper, applies = applies, doesntApply = doesntApply}) =
  let (rangeApplies, rangeDoesNotApply) = updatePartRange p attr oper threshold
   in countPossibilities' rangeApplies applies + countPossibilities' rangeDoesNotApply doesntApply

countPossibilities' :: Maybe AttributeRanges -> RuleTree -> Int
countPossibilities' ranges rules = case ranges of
  Just values -> countPossibilities values rules
  Nothing -> 0

partTwo :: Workflows -> Int
partTwo workflows = countPossibilities part (createTree workflows)
  where
    part = fromList $ map (,(1, 4000)) "xmas"

main = do
  content <- readFile "../input.txt"
  let (workflows, parts) = parseInput content
  print $ partOne workflows parts
  print $ partTwo workflows
