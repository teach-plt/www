mult x y =
    if (y < 1) then 0 else if (y < 2) then x else (x + (mult x (y-1))) ;
fact = \x -> if (x < 3) then x else mul x (fact (x-1)) ; -- unknown mul
main = print (fact 6) ;
