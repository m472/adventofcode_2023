import Data.Char
import Data.List
import Debug.Trace (trace)

firstAndLast :: [a] -> [a]
firstAndLast xs = [head xs , last xs]

replaceNumberWords :: String -> String
replaceNumberWords [] = []
replaceNumberWords ('o':'n':'e':xs)         = '1' : replaceNumberWords xs
replaceNumberWords ('t':'w':'o':xs)         = '2' : replaceNumberWords xs
replaceNumberWords ('t':'h':'r':'e':'e':xs) = '3' : replaceNumberWords xs
replaceNumberWords ('f':'o':'u':'r':xs)     = '4' : replaceNumberWords xs
replaceNumberWords ('f':'i':'v':'e':xs)     = '5' : replaceNumberWords xs
replaceNumberWords ('s':'i':'x':xs)         = '6' : replaceNumberWords xs
replaceNumberWords ('s':'e':'v':'e':'n':xs) = '7' : replaceNumberWords xs
replaceNumberWords ('e':'i':'g':'h':'t':xs) = '8' : replaceNumberWords xs
replaceNumberWords ('n':'i':'n':'e':xs)     = '9' : replaceNumberWords xs
replaceNumberWords (x:xs)                   =  x  : replaceNumberWords xs

replaceNumberWords' :: String -> String
replaceNumberWords' [] = []
replaceNumberWords' ('e':'n':'o':xs)         = '1' : replaceNumberWords' xs
replaceNumberWords' ('o':'w':'t':xs)         = '2' : replaceNumberWords' xs
replaceNumberWords' ('e':'e':'r':'h':'t':xs) = '3' : replaceNumberWords' xs
replaceNumberWords' ('r':'u':'o':'f':xs)     = '4' : replaceNumberWords' xs
replaceNumberWords' ('e':'v':'i':'f':xs)     = '5' : replaceNumberWords' xs
replaceNumberWords' ('x':'i':'s':xs)         = '6' : replaceNumberWords' xs
replaceNumberWords' ('n':'e':'v':'e':'s':xs) = '7' : replaceNumberWords' xs
replaceNumberWords' ('t':'h':'g':'i':'e':xs) = '8' : replaceNumberWords' xs
replaceNumberWords' ('e':'n':'i':'n':xs)     = '9' : replaceNumberWords' xs
replaceNumberWords' (x:xs)                   =  x  : replaceNumberWords' xs

firstAndLastDigit :: String -> String
firstAndLastDigit s = [firstDigit, lastDigit]
    where
        firstDigit = head $ filter isDigit $ replaceNumberWords s
        lastDigit = head $ filter isDigit $ replaceNumberWords' $ reverse s

partTwo :: String -> IO ()
partTwo content = print $ sum $ map (read . firstAndLastDigit) $ lines content

main = do
    content <- readFile "../input.txt"
    --print $ sum $ map (read . firstAndLast . filter isDigit) $ lines content
    partTwo content
