import Data.Bool

main :: IO ()
main = do
    i <- readFile "input1.txt"
    let w = lines i
    let x = getZeros 50 0 w
    let y = getCount 50 0 w
    print x
    print y

-----------------------TASK 1-----------------------

parseInstr :: String -> Int
parseInstr i | head i == 'L' = - (read $ tail i)
             | otherwise = read $ tail i

getZeros :: Int -> Int -> [String] -> Int
getZeros s c [] = c
getZeros s c (w:ws) = getZeros s' (c + if s' == 0 then 1 else 0) ws
    where n = parseInstr w
          s' = mod (s + n) 100

-----------------------TASK 2-----------------------

getCount :: Int -> Int -> [String] -> Int
getCount s c [] = c
getCount s c (w:ws) = getCount (mod s' 100) (c + counting s s') ws
    where n = parseInstr w
          s' = s + n

counting s s' | s' <= 0 = bool 0 (-1) (s==0) - div (s'- 1) 100
              | otherwise = div s' 100
