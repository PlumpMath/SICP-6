accumulate op initial s
    | null s = initial
    | otherwise = op (head s) (accumulate op initial (tail s))
    
horner_eval :: Double -> [Int] -> Double
horner_eval x coeff_seq = accumulate (\this_coeff higher_terms -> fromIntegral(this_coeff) + x * higher_terms ) 0.0 coeff_seq

main = do
    print $ horner_eval 2.0 [1, 3, 0, 5, 0, 1]