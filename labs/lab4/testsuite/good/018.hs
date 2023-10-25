one = 1 ;
main = print ((\ x -> (\ y -> (\ z -> z + 1) y) x) one) ;
