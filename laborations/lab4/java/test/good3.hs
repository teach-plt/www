summation = \ x -> if (x < 1) then x else (x + summation (x-1)) ;
main = print (summation 100) ; -- result 5050
