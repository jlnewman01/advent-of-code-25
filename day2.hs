import Data.Set (fromList, toList)

eg = "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"

main :: IO ()
main = do
    i <- readFile "input2.txt"
    -- let i = eg
    let x = splitWords (== ',') (filter (/= '\n') i)
    let y = concatMap (filter (\(x,y) -> even (length (show x))) . splitRange . rangeParse) x
    let task1 = sum $ concatMap getInvalidFromRange y
    let task2 = sum . removeRepeats $ getInvalids $ concatMap (splitRange . rangeParse) x
    print task1
    print task2

-- TASK 1 --

rangeParse ::  (Num a, Read a) => [Char] -> (a, a)
rangeParse x = (head ns, ns !! 1)
    where ns = map read $ splitWords (=='-') x

getInvalidFromRange ::  (Int,Int) -> [Int]
getInvalidFromRange (l',h') | ceiling (l/d') - floor (h/d') > 0 = []
                          | otherwise = map (d *) [ceiling (l/d') .. floor (h/d')]
    where d = divisor (length $ show l')
          d' = fromIntegral d
          l = fromIntegral l'
          h = fromIntegral h'

splitRange :: (Int, Int) -> [(Int, Int)]
splitRange (d1,d2) | l == length (show d2) = [(d1,d2)]
                   | otherwise = (d1, read (replicate l '9')): splitRange (read ("1" ++ replicate l '0'), d2)
    where l = length (show d1)

-- No. with 2n digits is symmetric iff it is divisible by the no with digits 10^(n-1)1
divisor :: Int -> Int
divisor n = read $ "1" ++ replicate ((n `div` 2) - 1) '0' ++ "1"

splitWords :: (Char -> Bool) -> String -> [String]
splitWords p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : splitWords p s''
                            where (w, s'') = break p s'

-- TASK 2 --

--No with digits mn is m-symmetric iff it is divisible by the no with digits (1 0^(n-1))^(m-1) 1

mDivisor :: Int -> Int -> Int
mDivisor m n = read $ foldr (\x acc -> [x] ++ replicate ((n `div` m) - 1) '0' ++ acc) "1" (replicate (m-1) '1')

validmDivisors :: Int -> [Int]
validmDivisors n = filter (\x -> n `mod` x == 0) [2.. n]

getInvalidFromRangeAndDivisor ::  (Int,Int) -> Int -> [Int]
getInvalidFromRangeAndDivisor (l',h') m | ceiling (l/d') - floor (h/d') > 0 = []
                                        | otherwise = map (d *) [ceiling (l/d') .. floor (h/d')]
    where d = mDivisor m (length $ show l')
          d' = fromIntegral d
          l = fromIntegral l'
          h = fromIntegral h'

getInvalids :: [(Int,Int)] -> [Int]
getInvalids rs = do
                (l,h) <- rs
                let xs = validmDivisors (length $ show l)
                x <- xs
                let y = getInvalidFromRangeAndDivisor (l,h) x
                y

removeRepeats ::(Eq a, Ord a) => [a] -> [a]
removeRepeats = toList . fromList
