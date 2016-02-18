import Data.Typeable

data MyList a = MyList [MyList a] | Element a deriving (Show)

deep_reverse a = case a of
    Element b -> Element b
    MyList b -> MyList ((reverse $ map deep_reverse $ tail b) ++ [deep_reverse $ head b])

main = do
    let a = MyList [MyList[Element 0, MyList [Element 1, Element 2]], MyList [Element 3, Element 4], MyList [Element 5, Element 6]]
    print a
    print $ deep_reverse a