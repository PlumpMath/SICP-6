data Tree a = Tree [Tree a] | Leaf a deriving (Show)

map_tree f a = case a of
    Leaf b -> Leaf (f b)
    Tree b -> Tree $ map (map_tree f) b
    
square_tree = map_tree (\x -> x * x)

main = do
    let a = Tree [Tree[Leaf 0, Tree [Leaf 1, Leaf 2]], Tree [Leaf 3, Leaf 4], Tree [Leaf 5, Leaf 6]]
    print a
    print $ square_tree a