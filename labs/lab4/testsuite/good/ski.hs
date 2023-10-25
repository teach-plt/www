-- SKI combinator calculus and the derivation of I
konst x y = x;
subst f g x = f x (g x);

id1 = subst konst konst;
id2 = subst konst subst;
id3 = subst konst id3;

test = id1 id2 id3 (konst 16) 42;

main = print (test);
