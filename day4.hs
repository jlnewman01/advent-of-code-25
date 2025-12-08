import Data.Bool
import Data.Char
import Data.List

main :: IO ()
main = do
    i <- readFile "input4.txt"
    let w = lines i
    let indices = filter (\x -> ind w x == '@') $ cartesianProd [0..(length w - 1)] [0..(length (head w) - 1)]
    let t1 = sum $ map ((\x -> bool 0 1 (x<4)) . countOccurences '@' . atInd w . flip getSurrounding w) indices
    let t2 = diffNum w (fixpoint removeRolls w)
    print t1
    print t2

--Task 1

cartesianProd :: [a] -> [b] -> [(a, b)]
cartesianProd xs ys = [(x,y) | x <- xs, y <- ys]

countOccurences :: Char -> String -> Int
countOccurences c = foldr (\x acc -> bool acc (acc + 1) (x==c)) 0

atInd :: [String] -> [(Int,Int)] -> String
atInd xs = map (ind xs)

getSurrounding :: (Int,Int) -> [String] -> [(Int,Int)]
getSurrounding (r,c) xss = filter inGrid [(r-1,c-1), (r,c-1), (r+1, c-1), (r-1, c), (r+1,c),(r-1,c+1),(r,c+1), (r+1, c + 1)]
    where inGrid (r',c') = r' `elem` [0..(length xss - 1)] && c' `elem` [0..(length (head xss) - 1)]

ind :: [String] -> (Int,Int) -> Char
ind xss (r,c) = (xss !! r) !! c

--Task 2

fixpoint :: (Eq a) => (a -> a) -> a -> a
fixpoint f x | y == x = y
             | otherwise = fixpoint f (f x)
    where y = f x

removeRolls :: [String] -> [String]
removeRolls xs = chunks (cs+1) $ map ((\e@(y, (c, r)) -> bool y '.' (checkAccessible e)) . \x -> (ind xs x, x)) $ cartesianProd [0..rs] [0..cs]
    where checkAccessible (y, (c, r)) = y == '@' && countOccurences '@' (atInd xs (getSurrounding (c,r) xs)) < 4
          cs = length (head xs) - 1
          rs = length xs - 1

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs =
    let (ys, zs) = splitAt n xs
    in  ys : chunks n zs

diffNum :: [String] -> [String] -> Int
diffNum [] _ = 0
diffNum ([]:xss) (_:yss) = diffNum xss yss
diffNum ((x:xs):xss) ((y:ys):yss) = bool 1 0 (x == y) + diffNum (xs:xss) (ys:yss)
