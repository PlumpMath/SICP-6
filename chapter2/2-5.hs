get_exp :: Integer -> Integer -> Integer
get_exp n k = get_exp_iter n k 0
    where get_exp_iter n k p
            | n == 1 = p
            | otherwise = get_exp_iter (n `div` k) k (p + 1)

cons :: Integer -> Integer -> Integer
cons x y = (2 ^ x) * (3 ^ y)

car :: Integer -> Integer
car z
    | z `rem` 3 /= 0 = get_exp z 2
    | otherwise = car (z `div` 3)

cdr :: Integer -> Integer
cdr z
    | z `rem` 2 /= 0 = get_exp z 3
    | otherwise = cdr (z `div` 2)
    
main = do
    print $ cons 1 2
    print $ car $ cons 1 2
    print $ cdr $ cons 1 2