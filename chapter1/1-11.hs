f n
  | n < 3 = n
  | otherwise = f (n-1) + (2 * f (n-2)) + (3 * f (n-3))
  
f_iter n = f_iter_inner 0 1 2 n
    where f_iter_inner a b c 0 = a
          f_iter_inner a b c 1 = b
          f_iter_inner a b c 2 = c
          f_iter_inner a b c m = f_iter_inner b c (3 * a + 2 * b + c) (m-1)

main = do
    n <- getLine
    print $ f (read n :: Integer)
    print $ f_iter (read n :: Integer)