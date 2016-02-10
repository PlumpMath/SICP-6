sum_by_term :: (Int -> Double) -> Int -> (Int -> Int) -> Int -> Double
sum_by_term term a next b = iter a 0
    where iter a result
            | a > b = result
            | otherwise = iter (next a) (result + (term a))
    
simpson :: (Double -> Double) -> Double -> Double -> Int -> Double
simpson f a b n = h / 3.0 * (sum_by_term simpson_term 0 next (n-2))
    where simpson_term k = (y k) + 4 * (y (k+1)) + (y (k+2))
          y k = f (a + (fromIntegral k) * h)
          h = (b - a) / (fromIntegral n)
          next k = k + 2

main = do
    n <- getLine
    print $ simpson (\x -> x^3 ) 0 1 (read n :: Int)