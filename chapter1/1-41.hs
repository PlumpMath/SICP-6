double f = \x -> f (f x)
inc x = x + 1
main = do
    print $ ((double (double double)) inc) 5