import Data.List
import Data.Set (fromList, toList)

eg :: [(Int, Int)]
eg = [(3,5),(10,14),(16,20),(12,18)]

main :: IO()
main = do
    i <- readFile "input5-1.txt"
    j <- readFile "input5-2.txt"
    let rs = map parseRange $ lines i
    let ids = map parseID $ lines j
    let fresh = filter (`inRanges` rs) ids
    let totalNo = sum $ map noInRange rs
    let mega = buildMegaRange rs
    print $ length fresh
    print $ megaRangeSum 0 0 mega

---TASK 1---

parseRange :: String -> (Int,Int)
parseRange s = (read $ head bounds, read $ bounds !! 1)
    where bounds = wordsWhen (=='-') s

parseID :: String -> Int
parseID = read

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

inRanges :: Int -> [(Int,Int)] -> Bool
inRanges i = any (\(l,u) -> i >= l && i <= u)

---TASK 2---

--Idea: Store the interval bounds in numerical order; encode lower bounds as '(' and upper as ')'
-- e.g. 3-6, 5-7 becomes:
-- _ _ _ ( _ ( ) ) 
-- 0 1 2 3 4 5 6 7
-- To get total no. in ranges we only need to parse the outermost brackets

expandRange :: (Int, Int) -> [Int]
expandRange (x,y) = [x .. y]

removeRepeats :: [Int] -> [Int]
removeRepeats = toList . fromList

allIDs :: [(Int,Int)] -> [Int]
allIDs = foldr (\x acc -> removeRepeats $ expandRange x ++ acc) []

buildMegaRange :: [(Int,Int)] -> [(Int,Char)]
buildMegaRange = foldr (\(x,y) acc -> megaRangeIns (megaRangeIns acc (x,'l')) (y,'h')) []

megaRangeIns :: [(Int,Char)] -> (Int, Char) -> [(Int,Char)]
megaRangeIns [] x = [x]
megaRangeIns (a@(x,_):rs) b@(y,t) | y < x  || (y == x && t == 'l') = b:a:rs
                                  | otherwise = a : megaRangeIns rs b

megaRangeSum :: Int -> Int -> [(Int,Char)] -> Int
megaRangeSum n p [] = 0
megaRangeSum 0 p ((x, 'l'):rs) = megaRangeSum 1 x rs
megaRangeSum 1 p ((x, 'h'):rs) = (x-p) + 1 + megaRangeSum 0 p rs
megaRangeSum n p ((x,'l'):rs) = megaRangeSum (n+1) p rs
megaRangeSum n p ((x, 'h'):rs) = megaRangeSum (n-1) p rs

intrsect :: (Int,Int) -> (Int, Int) -> (Int,Int)
intrsect x (-2,-2) = x
intrsect (-1,-1) _ = (-1,-1)
intrsect _ (-1,-1) = (-1,-1)
intrsect (a,b) (c,d) | a <= c && b >= c = (c, min b d)
                     | c < a && d >= a = (a, min d b)
                     | otherwise = (-1,-1)

noInRange :: (Int,Int) -> Int
noInRange (-1,-1) = 0
noInRange (l,h) = h - l + 1
