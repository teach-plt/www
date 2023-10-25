inc x = x + 1 ;
main = print ((\ f -> (\ g -> (\h -> h 1) g) f) inc) ;
