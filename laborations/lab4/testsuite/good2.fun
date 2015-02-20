print x = x ;

double x = x + x ;
twice f x = f (f x) ;
quadruple = twice double ;
mainz = print (twice quadruple 2) ;
maint = print (twice (\x -> x + double x) 6) ;
   
grow x = 1 + grow x ;

first x y = x ;

main = first 5 (grow 4) ; -- result 5 with -n, loop otherwise
