sumupto x =  if (x < 1) then x else (x + sumupto (x-1)) ;
main = print (sumupto 100) ; -- result 5050
