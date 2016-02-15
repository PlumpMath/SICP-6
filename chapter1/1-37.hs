cont_frac n d k = cont_frac_recur n d k 1
    where cont_frac_recur n d k l
                | k == l = (n k) / (d k)
                | otherwise = (n l) / (d l + (cont_frac_recur n d k (l+1)))

cont_frac2 n d k = cont_frac_iter n d k 0
    where cont_frac_iter n d k s
                | k == 0 = s
                | otherwise = cont_frac_iter n d (k-1) ((n k) / ((d k) + s))
                
tan_cf x k = cont_frac (\i -> if i == 1 then x else -x**i) (\i -> i*2-1) 10

main = do
    k <- getLine
    print $ cont_frac (\x -> 1.0) (\x -> 1.0) (read k :: Int)
    print $ cont_frac2 (\x -> 1.0) (\x -> 1.0) (read k :: Int)
    print $ tan 1.0
    print $ tan_cf 1.0 (read k :: Int)