-- Self-application will not type check
sapp f = f f;
two x = 2;
main = print (sapp two 1) ;
