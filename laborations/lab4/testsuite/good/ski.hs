-- SKI combinator calculus and the derivation of I
konst x y = x;
subst f g x = f x (g x);

sapp = subst id1 id2;

id1 = subst konst konst;
id2 = subst konst subst;
id3 = subst konst id3;

main = print (sapp id3 16);
