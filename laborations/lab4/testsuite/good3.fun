sum x =  if (x < 1) then x else (x + sum (x-1)) ;
main = sum 100 ; -- result 5050

