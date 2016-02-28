accumulate :: (Num t) => (t -> t1 -> t1) -> t1 -> [t] -> t1
accumulate op initial s
    | null s = initial
    | otherwise = op (head s) (accumulate op initial (tail s))

accumulate_n :: (Num t) => (t -> t1 -> t1) -> t1 -> [[t]] -> [t1]
accumulate_n op initial seqs
    | null $ head seqs = []
    | otherwise = [accumulate op initial [head s | s <- seqs]] ++ (accumulate_n op initial [tail s | s <- seqs])

data Vector t = Vector {elements :: [t]} deriving (Show)
data Matrix t = Matrix {rows :: [Vector t]} deriving (Show)

dot_product :: (Num t) => Vector t -> Vector t -> t
dot_product u v = accumulate (+) 0 (zipWith (*) (elements u) (elements v))

matrix_vector_product :: (Num t) => Matrix t -> Vector t -> Vector t
matrix_vector_product m v = Vector $ map (\row -> dot_product row v) (rows m)

transpose_matrix :: (Num t) => Matrix t -> Matrix t
transpose_matrix m = Matrix $ map (\x -> Vector x) $ accumulate_n (\x y -> [x] ++ y) [] [elements row | row <- (rows m)]

matrix_matrix_product :: (Num t) => Matrix t -> Matrix t -> Matrix t
matrix_matrix_product m n = Matrix $ map (\x -> Vector $ map (\y -> dot_product x y) (rows n_trans)) (rows m)
    where n_trans = transpose_matrix n

zero_vector :: (Num t) => Int -> Vector t
zero_vector n = Vector $ take n $ cycle [0]

replaceNth n newVal (x:xs)
     | n == 0 = newVal:xs
     | otherwise = x:replaceNth (n-1) newVal xs

set_vector_element :: (Num t) => Vector t -> Int -> t -> Vector t
set_vector_element v i value = u
    where u = Vector $ replaceNth i value (elements v)

identity_matrix n = Matrix all_rows
    where all_rows = [make_row i n | i <- [0..(n-1)]]
            where make_row i n = Vector $ ((take i $ cycle [0]) ++ [1] ++ (take (n-i-1) $ cycle [0]))

main = do
    let a = Vector ([1, 2, 3, 4] :: [Int])
    let b = Vector ([4, 5, 6, 7] :: [Int])
    let c = Vector ([6, 7, 8, 9] :: [Int])
    let m = Matrix [a, b, c]
    print a
    print b
    print c
    print m
    print $ dot_product a b
    print $ matrix_vector_product m (Vector [1, 2, 3])
    print $ transpose_matrix m
    print $ identity_matrix 4
    print $ matrix_matrix_product m $ identity_matrix 4