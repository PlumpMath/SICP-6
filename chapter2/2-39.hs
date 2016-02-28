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

reverse_fold_right = fold_right (\x y -> y ++ [x]) []
reverse_fold_left = fold_left (\x y -> [y] ++ x) []

main = do
    print $ reverse_fold_right [1..5]
    print $ reverse_fold_left [1..5]
    