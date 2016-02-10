sum_by_term term a next b
    | a > b = 0
    | otherwise = (term a) + (sum_by_term term (next a) next b)
    
simpson f a b n = h / 3.0 * (sum_by_term simpson_term 0 next (n-2))
    where simpson_term k = (y k) + 4 * (y (k+1)) + (y (k+2))
          y k = f (a + k * h)
          h = (b - a) / n
          next k = k + 2

main = do
    print $ simpson (\x -> x^3) 0 1 10