-- 2016-12-08
-- This one can go wrong under call-by-name/value with mutable environments

main = print ((\ x -> (\ f -> (\ x -> f x) 0) (\ y -> x - 1)) 2) ;

-- Expected output: 1
