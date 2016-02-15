data Rational = Rational { numer:: Integer, denom:: Integer } deriving (Show)

make_rat n d = Rational (n `div` g) (d `div` g)
    where g = gcd n d
add_rat x y = Rational ((numer x) * (denom y) + (numer y) * (denom x)) ((denom x) * (denom y))
sub_rat x y = Rational ((numer x) * (denom y) - (numer y) * (denom x)) ((denom x) * (denom y))
mul_rat x y = Rational ((numer x) * (numer y)) ((denom x) * (denom y))
div_rat x y = Rational ((numer x) * (denom y)) ((denom x) * (numer y))
equal_rat x y = (numer x) * (denom y) == (numer y) * (denom x)

main = do
    let x = Rational 1 3
    let y = Rational 2 5
    print x
    print y
    print $ add_rat x y
    print $ sub_rat x y
    print $ mul_rat x y
    print $ div_rat x y
    print $ equal_rat x y
    print $ make_rat 2 4