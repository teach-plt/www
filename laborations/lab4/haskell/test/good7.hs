true = 1 ;
false = 0 ;
even x = if (x < 1) then true else odd (x-1) ;
odd x = if (x < 1) then false else even (x-1) ;
main = print (odd 111111) ; -- result 1
