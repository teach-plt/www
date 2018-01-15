-- Self-application will not type check
sapp f = f f;

id x = x;

main = print (sapp id 1);
