x = \ x -> 600 ;  -- pointless but syntactically correct
id x = x ;
twice f x = f (f x) ;
fst x y = x;
fst1 = (\ x -> \ y -> x) ;
snd x y = y ;
main = print (twice (\x -> x) 6) ; -- result 7
-- main = print (fst (twice (\x -> x)) 6 7) ; -- result 7
