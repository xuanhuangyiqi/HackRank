data BTree = Leaf (Int, Int, Int)
    | Fork BTree BTree (Int, Int, Int)

minV :: BTree -> Int
minV (Leaf a) = let (_,_,mm) = a in mm
minV (Fork lt rt a) = let (_,_,mm) = a in mm

nodeV :: BTree -> (Int, Int, Int)
nodeV (Leaf a) = a
nodeV (Fork t1 t2 a) = a
lTree (Fork t1 t2 a) = t1
rTree (Fork t1 t2 a) = t2

mTree :: Int -> Int -> [Int] -> BTree
mTree x y z
    | x == y    = Leaf (x, y, z!!x)
    | otherwise = Fork lt rt (x, y, (min ml mr))
        where 
            m = div (x + y ) 2
            lt = mTree x m z
            rt = mTree (m+1) y z
            ml = minV lt
            mr = minV rt

askM :: Int -> Int -> BTree -> Int
askM x y t
    | x == l && y == r  = mm
    | y <= mid          = askM x y lt
    | x > mid           = askM x y rt
    | otherwise         = min (askM x mid lt) (askM (mid+1) y rt)
        where
            (l, r, mm) = nodeV t
            mid = div (l + r ) 2
            lt = lTree t
            rt = rTree t

conInput' :: String -> [Int]
conInput' st = [read x :: Int | x <- (words st)]

solve :: [Int] -> BTree -> Int
solve li tree = askM x y tree
    where
        x = li!!0
        y = li!!1

query numm tree = do
    if numm > 0 then do
        q <- getLine
        print (solve (conInput' q) tree)
        query (numm - 1) tree
    else return ()

main = do
    mn <- getLine
    li <- getLine
    query (head (tail (conInput' mn))) (mTree 0 (length (conInput' li)) (conInput' li))
    
