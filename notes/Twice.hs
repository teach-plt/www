double x     = x + x
comp   f g x = f (g x)
twice  f     = comp f f

-- call-by-value (cbv)
t0  = twice twice double 2
t1  = comp twice twice double 2
t2  = twice (twice double) 2
t3  = twice (comp double double) 2
t4  = comp (comp double double) (comp double double) 2
t5  = comp double double (comp double double 2)
t6  = comp double double (double (double 2))
t7  = comp double double (double (2 + 2))
t8  = comp double double (double 4)
t9  = comp double double (4 + 4)
t10 = comp double double 8
t11 = double (double 8)
t12 = double (8 + 8)
t13 = double 16
t14 = 16 + 16
t15 = 32

test = [t0,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15]


-- call-by-name (cbn)
u0  = twice twice double 2
u1  = comp twice twice double 2
u2  = twice (twice double) 2
u3  = comp (twice double) (twice double) 2
u4  = twice double (twice double 2)
u5  = comp double double (twice double 2)
u6  = double (double (twice double 2))
u7  = double (twice double 2) + double (twice double 2)
u8  = (twice double 2 + twice double 2) + double (twice double 2)
u9  = (comp double double 2 + twice double 2) + double (twice double 2)
u10 = (double (double 2) + twice double 2) + double (twice double 2)
u11 = ((double 2 + double 2) + twice double 2) + double (twice double 2)
u12 = (((2 + 2) + double 2) + twice double 2) + double (twice double 2)
u13 = ((4 + double 2) + twice double 2) + double (twice double 2)
u14 = ((4 + (2 + 2)) + twice double 2) + double (twice double 2)
u15 = ((4 + 4) + twice double 2) + double (twice double 2)
u16 = (8 + twice double 2) + double (twice double 2)

test' = [u0,u1,u2,u3,u4,u5,u6,u7,u8,u9,u10,u11,u12,u13,u14,u15,u16]
