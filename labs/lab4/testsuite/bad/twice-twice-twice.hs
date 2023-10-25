comp f g x = f (g x);

twice f = comp f f;

double x = x + x;

main = print (twice twice twice 2);
