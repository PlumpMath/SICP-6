mul_fast b n = mul_fast_iter 0 b n
    where mul_fast_iter a b n
                    | n == 0 = a
                    | n `rem` 2 == 0 = mul_fast_iter a (b*2) (n `div` 2)
                    | otherwise = mul_fast_iter (a+b) (b*2) (n `div` 2)

main = do
    x <- getLine
    n <- getLine
    print $ mul_fast (read x :: Double) (read n :: Int)