max = 600 ;
id x = x ;
twice f x = f (f x) ;
fst x y = x ;
snd x y = y ;
--rep x = Pair x x ;

--main = twice rep 5 ;

main = fst (twice (\x -> x)) 6 7 ; -- result 7

--main = twice rep 5 ;

--main = twice rep (snd (fst (twice id 5) 7) 6) ;
