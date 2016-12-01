-- Church numerals and their relation to integers

church0 = \f -> \x -> x ;
churchS = \n -> \f -> \x -> f (n f x) ;
church2int n = n (\i -> i + 1) 0 ;
int2church i = if (i < 1) then church0 else churchS (int2church (i-1)) ;
main = print (church2int (int2church (church2int (int2church 8)))) ;  -- result 8
