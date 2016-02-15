iterative_improve good_enough improve_guess = \x -> if good_enough x then x else iterative_improve good_enough improve_guess (improve_guess x)

fixed_point f = iterative_improve (\x -> abs(x - f x) < 1e-6) (\x -> f x)
sqrt_f = \y -> iterative_improve (\x -> abs(x*x - y) < 1e-6) (\x -> (x + y/x) * 0.5) 1.0

main = do
    print $ sqrt_f 4.0
    print $ fixed_point cos 1.0