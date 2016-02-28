accumulate op initial s
    | null s = initial
    | otherwise = op (head s) (accumulate op initial (tail s))

accumulate_n op initial seqs
    | null $ head seqs = []
    | otherwise = [accumulate op initial [head s | s <- seqs]] ++ (accumulate_n op initial [tail s | s <- seqs])


flat_map proc s = accumulate (++) [] (map proc s)

is_prime :: Int -> Bool
is_prime x = foldr (\i res -> ((x `rem` i) /= 0) && res) True [2..(x-1)]

prime_sum :: [Int] -> Bool
prime_sum p = is_prime $ ((p !! 0) + (p !! 1))

make_pair_sum :: [Int] -> ((Int, Int), Int)
make_pair_sum p = ((p!!0, p!!1), ((p !! 0) + (p !! 1)))

unique_pairs :: Int -> [[Int]]
unique_pairs n = flat_map (\i -> map (\j -> [i, j]) [1..(i-1)]) [1..n] 

prime_sum_pairs :: Int -> [((Int, Int), Int)]
prime_sum_pairs n = map make_pair_sum $ filter prime_sum $ unique_pairs n

remove :: Int -> [Int] -> [Int]
remove x s = filter (\y -> y /= x) s

permutations :: [Int] -> [[Int]]
permutations s
    | null s = [[]]
    | otherwise = flat_map (\x -> map (\p -> [x] ++ p) $ permutations (remove x s)) s

main = do
    print $ unique_pairs 10
    print $ prime_sum_pairs 10
    print $ permutations [1..5]