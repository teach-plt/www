-- f :: a -> (Integer -> Integer) -> Integer
f = \ x -> \ x -> x 1 + 0 ;
-- g :: a -> Integer -> Integer
g = \ y -> \ y -> y + 1 ;
inc z = z + 1 ;
one = 1 ;
main = print (f one inc - g inc one) ;
