improve 2 guess x = 0.5 * (guess + (x / guess))
improve 3 guess x = (x / guess^2 + 2.0 * guess) / 3.0
good_enough n guess x = abs (guess^n - x) < 1e-6

sqrt_iter n guess x = if good_enough n guess x then guess else sqrt_iter n (improve n guess x) x

main = do
    x <- getLine
    n <- getLine
    print $ sqrt_iter (read n :: Int) 1.0 (read x :: Double)