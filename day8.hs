import Data.List (sortBy, sort)
import Data.Set (toList, fromList)

main :: IO ()
main = do
    i <- readFile "input8.txt"
    let w = lines i
    let is = map parseCoords w
    let eqs = sortPairsBySmallestDist $ coordPairs is is
    let cs = joinByEquivalenceRelation (map (: []) is) (take 1000 eqs)
    let t1 = product $ take 3 $ reverse $ sort $ map length cs
    let t2 = joinUntilClosed (map (: []) is) eqs ((0,0,0),(0,0,0))
    print t1
    print t2

---TASK 1---

type Coord = (Int,Int,Int)

joinByEquivalenceRelation :: [[Coord]] -> [(Coord,Coord)] -> [[Coord]]
joinByEquivalenceRelation xss [] = xss
joinByEquivalenceRelation xss ((x,y):es) = joinByEquivalenceRelation (joined : removed) es
    where ml1 = findIndex (x `elem`) xss 0
          ml2 = findIndex (y `elem`) xss 0
          joined = removeRepeats $ xss !! ml1 ++ xss !! ml2
          removed | ml1 < ml2 = removeAtIndex ml1 $ removeAtIndex ml2 xss
                  | ml1 > ml2 = removeAtIndex ml2 $ removeAtIndex ml1 xss
                  | ml1 == ml2 = removeAtIndex ml1 xss

findIndex :: (a -> Bool) -> [a] -> Int -> Int
findIndex p [] n = -1
findIndex p (x:xs) n | p x = n
                     | otherwise = findIndex p xs (n+1)

removeAtIndex :: Int -> [a] -> [a]
removeAtIndex n xs = take n xs ++ drop (n+1) xs

parseCoords :: String -> Coord
parseCoords s = (read $ is !! 0, read $ is !! 1, read $ is !! 2)
    where is = splitWords (==',') s

splitWords :: (Char -> Bool) -> String -> [String]
splitWords p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : splitWords p s''
                            where (w, s'') = break p s'

sortPairsBySmallestDist :: [(Coord,Coord)] -> [(Coord,Coord)]
sortPairsBySmallestDist = sortBy (\(u,v) (x,y) -> compare (dist (u,v)) (dist (x,y)))

dist :: (Coord,Coord) -> Int
dist ((u,v,w),(x,y,z)) = (u-x)^2 + (v-y)^2 + (w-z)^2

coordPairs :: [Coord] -> [Coord] -> [(Coord,Coord)]
coordPairs [] [] = []
coordPairs (x:xs) (_:ys) = zip (replicate (length ys) x) ys ++ coordPairs xs ys

removeRepeats :: [Coord] -> [Coord]
removeRepeats = toList . fromList

---TASK 2---

joinUntilClosed :: [[Coord]] -> [(Coord,Coord)] -> (Coord,Coord) -> Int
joinUntilClosed xss [] ((x1,_,_),(x2,_,_)) = x1 * x2
joinUntilClosed [xs] _ ((x1,_,_),(x2,_,_)) = x1 * x2
joinUntilClosed xss ((x,y):es) prevPair = joinUntilClosed (joined : removed) es (x,y)
    where ml1 = findIndex (x `elem`) xss 0
          ml2 = findIndex (y `elem`) xss 0
          joined = removeRepeats $ xss !! ml1 ++ xss !! ml2
          removed | ml1 < ml2 = removeAtIndex ml1 $ removeAtIndex ml2 xss
                  | ml1 > ml2 = removeAtIndex ml2 $ removeAtIndex ml1 xss
                  | ml1 == ml2 = removeAtIndex ml1 xss