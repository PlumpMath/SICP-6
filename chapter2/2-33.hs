accumulate op initial s
    | null s = initial
    | otherwise = op (head s) (accumulate op initial (tail s))
    

map' f = accumulate (\x y -> [f x] ++ y) []

append' s1 s2 = accumulate (\x y -> y ++ [x]) s1 s2

length' = accumulate (\x y -> 1 + y) 0

main = do
    print $ map' (*2) ([1..10] :: [Int])
    print $ append' ([1..10] :: [Int]) ([11..20] :: [Int])
    print $ length' [1..10]