id x = x;

-- some solutions make 2 into a _function_ closure under call-by-name
main = print (id 2 1);
