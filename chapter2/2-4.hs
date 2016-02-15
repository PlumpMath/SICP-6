cons x y = dispatch
    where dispatch m
            | m == 0 = x
            | m == 1 = y

car z = z 0
cdr z = z 1

cons' x y = \m -> m x y
car' z = z (\p q -> p)
cdr' z = z (\p q -> q)

main = do
    print $ car $ cons 1.0 2.0
    print $ cdr $ cons 1.0 2.0
    print $ car' $ cons' 1.0 2.0
    print $ cdr' $ cons' 1.0 2.0