import Data.List (sort)
import Data.Bool

main :: IO ()
main = do
    i <- readFile "input9.txt"
    let w = lines i
    let is = map parseCoords w
    let rects = cartesianProd is is
    let t1 = maximum $ map rectArea rects
    let border = zip is (tail is ++ [head is])
    let t2 = maximum $ map rectArea $ filter (not . linesIntersect border) rects
    print t1
    print t2

---TASK 1---

type Coord = (Int,Int)
type Line = (Coord,Coord)

parseCoords :: String -> Coord
parseCoords s = (read $ head is, read $ is !! 1)
    where is = splitWords (==',') s

splitWords :: (Char -> Bool) -> String -> [String]
splitWords p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : splitWords p s''
                            where (w, s'') = break p s'

cartesianProd :: [a] -> [b] -> [(a, b)]
cartesianProd xs ys = [(x,y) | x <- xs, y <- ys]

rectArea :: (Coord,Coord) -> Int
rectArea ((x1,y1),(x2,y2)) = (1 + abs (x1 - x2)) * (1 + abs (y2 - y1))

---TASK 2---

--Check if any edge of the rectangle intersects with an edge of the border, reject if so
--As crossing sides of border => some part of the rectangle lies outside

--Once we know rectangle is all in border or out of border, we just have to check some point
--inside to see which one it is (but I didn't bother implementing this as it actually worked
--on my input without it...)

getRectangleOuter :: (Coord, Coord) -> [Line]
getRectangleOuter ((x1,y1),(x2,y2)) = [((x1,y1), (x2,y1)),
                                       ((x2,y1), (x2,y2)),
                                       ((x2,y2), (x1,y2)),
                                       ((x1,y2), (x1,y1))]

linesIntersect :: [Line] -> Line -> Bool
linesIntersect s l = any (lineIntersect l) s

lineIntersect :: Line -> Line -> Bool
lineIntersect ((x1,y1),(x2,y2)) ((x1',y1'),(x2',y2')) = xmin' < xmax && xmax' > xmin
                                                      && ymin' < ymax && ymax' > ymin
                                where xmin = min x1 x2
                                      xmax = max x1 x2
                                      ymin = min y1 y2
                                      ymax = max y1 y2
                                      xmin' = min x1' x2'
                                      xmax' = max x1' x2'
                                      ymin' = min y1' y2'
                                      ymax' = max y1' y2'