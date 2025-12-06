import Data.List (transpose)
import Data.List.Split
import Data.Char
import Data.Bool

main :: IO ()
main = do
    i <- readFile "input6.txt"
    let w1 = map words $  lines i
    let w2 = splitOn ["     "] $ transpose $ lines i
    let t1 = sum $  map doOperation $ transpose w1
    let t2 = sum $ map (doOperation . doFormat) w2
    print t1
    print t2

---TASK 1---

doOperation :: [String] -> Int
doOperation xs | last xs == "+" = sum (map read $ init xs)
               | last xs == "*" = product (map read $ init xs)

---TASK 2---

doFormat :: [String] -> [String]
doFormat xs = map (filter isDigit) xs ++ [operation]
    where operation = bool "+" "*" (any (\x -> '*' `elem` x) xs)

