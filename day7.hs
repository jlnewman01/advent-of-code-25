import Data.Bool
import Data.Set (fromList, toList)
import Data.List (sortBy)
import Data.Function (on)

main :: IO()
main = do
    i <- readFile "input7.txt"
    let w = lines i
    let t1 = splitCount w [fstIndex (head w) 0 'S'] 0
    let t2 = timelineCount w [(fstIndex (head w) 0 'S',1)]
    print t1
    print t2

---TASK 1---

fstIndex :: (Eq a) => [a] -> Int -> a -> Int
fstIndex [] n e = n
fstIndex (x:xs) n e = bool (fstIndex xs (n+1) e) n (x == e)

removeRepeats :: [Int] -> [Int]
removeRepeats = toList . fromList

splitCount :: [String] -> [Int] -> Int -> Int
splitCount [] beamLocs c = c
splitCount (x : xs) beamLocs c = splitCount xs newBeamLocs (c + length hitSplit)
    where positions = zip [0.. length x - 1] x
          splitPos = map fst $ filter (\x -> snd x == '^') positions
          hitSplit = filter (`elem` splitPos) beamLocs
          newBeamLocs = removeRepeats $ filter (`notElem` splitPos) beamLocs ++
                        concatMap (\x -> [x+1,x-1]) hitSplit

---TASK 2---

--Keep track of multiplicities of beams

timelineCount :: [String] -> [(Int,Int)] -> Int
timelineCount [] beamLocs = sum $ map snd beamLocs
timelineCount (x : xs) beamLocs = timelineCount xs newBeamLocs
    where positions = zip [0.. length x - 1] x
          splitPos = map fst $ filter (\x -> snd x == '^') positions
          hitSplit = filter (\(x,y) -> x `elem` splitPos) beamLocs
          newBeamLocs = groupRepeats $ filter (\(x,y) -> x`notElem` splitPos) beamLocs ++
                        concatMap (\(x,y) -> [(x+1,y),(x-1,y)]) hitSplit

groupRepeats :: [(Int,Int)] -> [(Int,Int)]
groupRepeats is = foldl (\acc@((l1,r1):xs) h@(l2,r2) -> bool (h:acc) ((l1,r1+r2):xs) (l1 == l2))
         [head ordered] $ tail ordered
    where ordered = sortBy (flip compare `on` fst) is
