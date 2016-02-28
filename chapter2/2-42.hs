accumulate op initial s
    | null s = initial
    | otherwise = op (head s) (accumulate op initial (tail s))

accumulate_n op initial seqs
    | null $ head seqs = []
    | otherwise = [accumulate op initial [head s | s <- seqs]] ++ (accumulate_n op initial [tail s | s <- seqs])


flat_map proc s = accumulate (++) [] (map proc s)

safe :: Int -> [(Int, Int)] -> Bool
safe k positions = accumulate (\p res -> (is_safe p) && res) True $ tail positions
    where is_safe p = (fst p /= row0) && (abs(col0 - (snd p)) /= abs(row0 - (fst p)))
          row0 = fst (head positions)
          col0 = snd (head positions)

adjoin_position :: Int -> Int -> [(Int, Int)] -> [(Int, Int)]
adjoin_position new_row k rest_of_queens
        | null rest_of_queens = [(new_row, k)]
        | otherwise = rest_of_queens ++ [(new_row, k)]

queens board_size = queens_col board_size
    where queens_col k
            | k == 0 = [[]]
            | otherwise = filter (\positions -> safe k positions) $ flat_map (\rest_of_queens -> map (\new_row -> adjoin_position new_row k rest_of_queens) [1..board_size]) $ queens_col (k-1)

main = do
    print $ queens 3