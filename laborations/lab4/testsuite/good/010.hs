-- 2016-12-08
-- This one can go wrong under call-by-value with mutable environments

main = print ((\ x -> (\ y -> x) ((\ x -> x) 0)) 1) ;

-- Expected output: 1
