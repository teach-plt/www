doub x = x + x ;
pow x = if (x < 1) then 1 else doub (pow (x-1)) ;
main = print (pow 30) ;  -- 1073741824, takes a long time with -n without closures
