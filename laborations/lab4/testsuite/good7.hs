true = 1 ;
false = 0 ;
neven x = if (x < 1) then true else nodd (x-1) ;
nodd x = if (x < 1) then false else neven (x-1) ;
main = print (nodd 111111) ; -- result 1
