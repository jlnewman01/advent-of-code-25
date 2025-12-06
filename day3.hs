import Data.Bool
import Data.Char
import Data.List

main :: IO ()
main = do
    i <- readFile "input3.txt"
    let w = sum $ map (bestBattery . map digitToInt) (lines i)
    let x = sum $ map (flip bestBattery' 11 . map digitToInt) (lines i)
    print w
    print x

-----TASK 1-----

bestBattery :: [Int] -> Int
bestBattery xs = 10 * d1 + d2
        where n = fstIndex xs 0 d1
              d1 = highestNo (init xs)
              d2 = highestNo $ drop (n+1) xs

fstIndex :: [Int] -> Int -> Int -> Int
fstIndex [] n e = n
fstIndex (x:xs) n e = bool (fstIndex xs (n+1) e) n (x == e)

highestNo :: [Int] -> Int
highestNo = foldr (\x acc-> bool acc x (x > acc)) 1

-----TASK 2-----

dropLast :: Int -> [a] -> [a]
dropLast n xs = take (length xs - n) xs

bestBattery' :: [Int] -> Int -> Int
bestBattery' xs 0 = highestNo xs
bestBattery' xs m = 10^m * d + bestBattery' (drop (i+1) xs) (m-1)
        where d = highestNo $ dropLast m xs
              i = fstIndex xs 0 d