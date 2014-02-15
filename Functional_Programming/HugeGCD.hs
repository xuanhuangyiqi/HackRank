divi a = head [x | x <- [2..a], a `mod` x == 0]

divide' :: Int ->[Int]
divide' n
    | n == 1        = []
    | (divi n) == n = [n]
    | otherwise     = (divi n):(divide' (n `div` (divi n)))

cont' :: [Int] -> [Int] -> [Int]
cont' x y
    | x == [] = y
    | y == [] = x
    | head x > head y = (head y):(cont' x (tail y))
    | otherwise  = (head x):(cont' y (tail x))

conna' :: [Int] -> [Int]
conna' n
    | tail n == []  = divide' (head n)
    | otherwise     = cont' (divide' (head n)) (conna' (tail n))

common' :: [Int] -> [Int] -> Int
common' x y
    | x == [] || y == []    = 1
    | head x > head y       = common' x (tail y)
    | head x < head y       = common' (tail x) y
    | otherwise             = ((head x) * (common' (tail x) (tail y)) `mod` 1000000007)

conInput' :: String -> [Int]
conInput' st = [read x :: Int | x <- (words st)]

main = do   x <- getInt
            xs <- getLine
            y <- getInt
            ys <- getLine
            print (common' (conna' (conInput' xs)) (conna' (conInput' ys)))
getInt :: IO Int
getInt = readLn


