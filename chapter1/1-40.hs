fixed_point f first_guess = try first_guess
    where try guess = if close_enough guess next then next else try next
            where next = f guess
          close_enough v1 v2 = (abs (v1 - v2)) < 1e-6

deriv g = \x -> (g (x + dx) - g x) / dx
    where dx = 1e-6

newton_transform g = \x -> x - (g x) / ((deriv g) x)
newtons_method g guess = fixed_point (newton_transform g) guess
main = do
    let a = 1.0
    let b = 2.0
    let c = 3.0
    print $ newtons_method (cubic a b c) 1
        where cubic a b c = \x -> x ** 3 + a * (x ** 2) + b * x + c