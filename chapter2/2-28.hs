import Data.List

data MyList a = MyList [MyList a] | Element a deriving (Show)

fringe a = case a of
    Element b -> [Element b]
    MyList b -> concat ([fringe $ head b, concat $ map fringe $ tail b])

main = do
    let a = MyList [MyList[Element 0, MyList [Element 1, Element 2]], MyList [Element 3, Element 4], MyList [Element 5, Element 6]]
    print a
    print $ fringe a