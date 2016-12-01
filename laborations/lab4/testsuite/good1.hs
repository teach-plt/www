max = 600 ;
id x = x ;
twice f x = f (f x) ;
fst x y = x ;
snd x y = y ;
main = print (fst (twice (\x -> x)) 6 7) ; -- result 7
