accumulate :: (Num t) => (t -> t1 -> t1) -> t1 -> [t] -> t1
accumulate op initial s
    | null s = initial
    | otherwise = op (head s) (accumulate op initial (tail s))

accumulate_n :: (Num t) => (t -> t1 -> t1) -> t1 -> [[t]] -> [t1]
accumulate_n op initial seqs
    | null $ head seqs = []
    | otherwise = [accumulate op initial [head s | s <- seqs]] ++ (accumulate_n op initial [tail s | s <- seqs])
    
fold_left :: (Num t) => (t1 -> t -> t1) -> t1 -> [t] -> t1
fold_left op initial s = iter initial s
    where iter result rest
                | null rest = result
                | otherwise = iter (op result (head rest)) (tail rest)

fold_right = accumulate

main = do
    -- seems foldl and foldr in Haskell have the opposite meaning???
    print $ foldl (/) 1 [1..3]
    print $ foldr (/) 1 [1..3]
    print $ fold_right (/) 1 [1..3]
    print $ fold_left (/) 1 [1..3]
    
    print $ fold_right (\x y -> [x] ++ y) [] [1..3]
    print $ fold_left (\x y -> x ++ [y]) [] [1..3]
