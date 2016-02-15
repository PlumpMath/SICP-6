data Point = Point { x :: Double, y :: Double } deriving (Show)
data Segment = Segment { a :: Point, b :: Point } deriving (Show)

make_point x y = Point x y
make_segment a b = Segment a b
mid_point s = Point (0.5 * (x (a s) + x (b s))) (0.5 * (y (a s) + y (b s)))

main = do
    print $ Point 1.0 2.0
    print $ mid_point $ Segment (Point 1.0 2.0) (Point 3.0 1.0)