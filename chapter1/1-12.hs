f 1 = [1]
f n = zipWith (+) ([0]++g) (g++[0])
    where g = f (n-1)
    
main = do
    n <- getLine
    print $ f (read n :: Integer)