double = (\ x -> add x x)
comp   = (\ f -> \ g -> \ x -> f (g x))
twice  = (\ f -> @comp f f)

main = @twice @twice @double 2
