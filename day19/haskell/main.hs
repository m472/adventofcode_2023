import Data.Map (Map, fromList, (!))
import Debug.Trace (trace, traceShowId)

data Result = Accept | Reject | WorkflowReference String deriving (Eq, Show)

data Rule = Unconditional Result | Conditional [Char] Int (Part -> Int) (Int -> Int -> Bool) Result

data Part = Part {x :: Int, m :: Int, a :: Int, s :: Int} deriving (Show)

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

parseInput :: String -> (Map String [Rule], [Part])
parseInput s =
  let [workflows, parts] = splitOn [] $ lines s
   in (fromList (map parseWorkflow workflows), map parsePart parts)

applyRules :: Part -> [Rule] -> Result
applyRules _ ((Unconditional result) : rs) = result
applyRules part ((Conditional _ thr getter opr result) : rs)
  | getter part `opr` thr = result
  | otherwise = applyRules part rs

classify :: Map String [Rule] -> Part -> Result
classify = classify' "in"

classify' :: String -> Map String [Rule] -> Part -> Result
classify' name workflows part =
  let rules = workflows ! name
   in case applyRules part rules of
        Accept -> Accept
        Reject -> Reject
        WorkflowReference nextName -> classify' nextName workflows part

partSum :: Part -> Int
partSum p = x p + m p + a p + s p

partOne :: Map String [Rule] -> [Part] -> Int
partOne workflows parts = sum $ map partSum $ filter (\p -> classify workflows p == Accept) parts

main = do
  content <- readFile "../input.txt"
  let (workflows, parts) = parseInput content
  print $ partOne workflows parts

