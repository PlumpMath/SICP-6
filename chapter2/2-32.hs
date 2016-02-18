subsets s
    | null s = []
    | otherwise = if null a then [[], [head s]] else a ++ (map (\x -> [head s] ++ x) a)
        where a = subsets $ tail s

main = do
    print $ subsets [1..3]