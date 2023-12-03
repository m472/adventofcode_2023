import Data.Char
import Data.List

firstAndLast :: [a] -> [a]
firstAndLast xs = [head xs , last xs]

startsWith :: Eq a => [a] -> [a] -> Bool
startsWith [] b = True
startsWith a [] = False
startsWith (a:as) (b:bs) = (a == b) && startsWith as bs

{-
replaceWords :: String -> String -> String
replaceWords pattern replacement s = replaceWords' pattern replacement [] s

replaceWords' :: String -> String -> String -> String
replaceWords' [] replacement acc [] = replacement
replaceWords' _ replacement acc [] = acc
replaceWords' [] replacement acc s = replacement : replaceWords' acc replacement [] s
replaceWords' (p:ps) replacement acc (x:xs) = if p == x then replaceWords' ps replacement (x:acc) xs else x:(replaceWords' (p:ps) replacement [] xs)
-}

replaceNumberWords :: String -> String
replaceNumberWords ('o':'n':'e':xs) =         '1' : replaceNumberWords xs
replaceNumberWords ('t':'w':'o':xs) =         '2' : replaceNumberWords xs
replaceNumberWords ('t':'h':'r':'e':'e':xs) = '3' : replaceNumberWords xs
replaceNumberWords ('f':'o':'u':'r':xs) =     '4' : replaceNumberWords xs
replaceNumberWords ('f':'i':'v':'e':xs) =     '5' : replaceNumberWords xs
replaceNumberWords ('s':'i':'x':xs) =         '6' : replaceNumberWords xs
replaceNumberWords ('s':'e':'v':'e':'n':xs) = '7' : replaceNumberWords xs
replaceNumberWords ('e':'i':'g':'h':'t':xs) = '8' : replaceNumberWords xs
replaceNumberWords ('n':'i':'n':'e':xs) =     '9' : replaceNumberWords xs
replaceNumberWords (x:xs) =                     x : replaceNumberWords xs
replaceNumberWords [] = []

main = do
    content <- readFile "../input.txt"
    let numbers = (map (read . firstAndLast . filter isNumber . replaceNumberWords) $ lines content) :: [Int]
    let a = (map (read . firstAndLast . filter isNumber . replaceNumberWords) $ lines content)::[Integer]
    print $ sum a 
    --print $ sum numbers
