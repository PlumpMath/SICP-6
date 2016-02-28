accumulate op initial s
    | null s = initial
    | otherwise = op (head s) (accumulate op initial (tail s))

accumulate_n op initial seqs
    | null $ head seqs = []
    | otherwise = [accumulate op initial [head s | s <- seqs]] ++ (accumulate_n op initial [tail s | s <- seqs])


flat_map proc s = accumulate (++) [] (map proc s)

safe :: [(Int, Int)] -> Bool
safe positions = accumulate (\p res -> (is_safe p) && res) True $ tail positions
    where is_safe p = (fst p /= row0) && (abs(col0 - (snd p)) /= abs(row0 - (fst p)))
          row0 = fst (head positions)
          col0 = snd (head positions)

adjoin_position :: [(Int, Int)] -> Int -> Int -> [(Int, Int)]
adjoin_position rest_of_queens col new_row
        | null rest_of_queens = [(new_row, col)]
        | otherwise = rest_of_queens ++ [(new_row, col)]

queens board_size = queens_col board_size
    where queens_col k
            | k == 0 = [[]]
            | otherwise = filter safe $ flat_map (propose_new_configuration k) $ queens_col (k-1)
                where propose_new_configuration k rest_of_queens = map (adjoin_position rest_of_queens k) [1..board_size]

main = do
    n <- getLine
    mapM_ print $ queens (read n::Int)