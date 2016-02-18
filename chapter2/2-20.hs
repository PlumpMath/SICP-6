same_parity (x:xs) = [x] ++ find_all (if (odd x) then odd else even) xs
    where find_all _ [] = []
          find_all pred (y:ys) = (if (pred y) then [y] else []) ++ find_all pred ys

main = do
    print $ same_parity [1..7]
    print $ same_parity [2..7]