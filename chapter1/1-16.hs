exp_fast b n = exp_fast_iter 1 b n
    where exp_fast_iter a b n
                    | n == 0 = a
                    | n `rem` 2 == 0 = exp_fast_iter a (b*b) (n `div` 2)
                    | otherwise = exp_fast_iter (a*b) (b*b) (n `div` 2)

main = do
    x <- getLine
    n <- getLine
    print $ exp_fast (read x :: Double) (read n :: Int)