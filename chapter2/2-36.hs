accumulate :: (Num t) => (t -> t -> t) -> t -> [t] -> t 
accumulate op initial s
    | null s = initial
    | otherwise = op (head s) (accumulate op initial (tail s))

accumulate_n :: (Num t) => (t -> t -> t) -> t -> [[t]] -> [t]
accumulate_n op initial seqs
    | null $ head seqs = []
    | otherwise = [accumulate op initial [head s | s <- seqs]] ++ (accumulate_n op initial [tail s | s <- seqs])

main = do
    print $ accumulate_n (+) 0 [[1, 2, 3], [4, 5, 6], [7, 8, 9], [10, 11, 12]]