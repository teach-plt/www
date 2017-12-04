max = 600 ;
id x = x ;
twice f x = f (f x) ;
first x y = x ;
second x y = y ;
main = print (first (twice (\x -> x)) 6 7) ; -- result 7
