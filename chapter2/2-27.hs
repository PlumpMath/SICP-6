import Data.Typeable

data MyList a = MyList [MyList a] | Element a deriving (Show)

deep_reverse a = case a of
    Element b -> Element b
    MyList b -> case b of
        x:xs -> MyList ((deep_reverse_helper xs) ++ [deep_reverse x])
        where deep_reverse_helper y
                | null y = []
                | otherwise = (deep_reverse_helper $ tail y) ++ [deep_reverse $ head y]

main = do
    let a = MyList [MyList[Element 0, MyList [Element 1, Element 2]], MyList [Element 3, Element 4], MyList [Element 5, Element 6]]
    print a
    print $ deep_reverse a