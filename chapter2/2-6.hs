zero = \f -> (\x -> x)
one = \f -> (\x -> f x)
two = \f -> (\x -> f $ f x)
add_1 n = \f -> (\x -> f ((n f) x))

add m n = \f -> (\x -> (m f) ((n f) x))

main = do
    print $ zero (\x -> x) 1024
    print $ one succ 1024
    print $ two succ 1024