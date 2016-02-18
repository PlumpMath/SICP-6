--map' :: (t -> t) -> [t] -> [t]
map' f items = iter items []
    where iter things answer
            | null things = answer
            | otherwise = iter (tail things) (answer ++ [f $ head things])

square_list = map' (\x -> x * x)

main = do
    print $ square_list ([1..7] :: [Int])
