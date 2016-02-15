fixed_point f first_guess = try first_guess
    where try guess = if close_enough guess next then next else try next
            where next = f guess
          close_enough v1 v2 = (abs (v1 - v2)) < 1e-6

main = do
    print $ fixed_point (\x -> 1 + 1/x) 1.0
    print $ fixed_point (\x -> log(1000)/log(x)) 10.0
