comp_fast b n initval doubling havling adding = comp_fast_iter initval b n
    where comp_fast_iter a b n
                    | n == 0 = a
                    | n `rem` 2 == 0 = comp_fast_iter a (doubling b) (havling n)
                    | otherwise = comp_fast_iter (adding a b) (doubling b) (havling n)

main = do
    x <- getLine
    n <- getLine
    putStr (x ++ " * " ++ n ++ " = ")
    print $ comp_fast (read x :: Double) (read n :: Int) 0 (\x -> x * 2) (\x -> x `div` 2) (+)
    putStr (x ++ " ^ " ++ n ++ " = ")
    print $ comp_fast (read x :: Double) (read n :: Int) 1 (\x -> x * x) (\x -> x `div` 2) (*)