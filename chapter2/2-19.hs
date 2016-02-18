cc amount coin_values
    | amount == 0 = 1
    | (amount < 0) || (no_more coin_values) = 0
    | otherwise = (cc amount (tail coin_values)) + (cc (amount - head coin_values) coin_values)
    where no_more x = null coin_values

main = do
    let us_coins = [50, 25, 10, 5, 1]
    let uk_coins = [100, 50, 20, 10, 5, 2, 1, 0.5]
    print $ cc 100 us_coins
    print $ cc 100 uk_coins