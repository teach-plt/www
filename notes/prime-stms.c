// TYPE(VARS): expression of type TYPE over variables VARS
bool divides (int q, int p) {
  return BOOL(p,q);
}

//
bool prime (int p) {
  if (BOOL(p)) return BOOL(p);
  else {
    int q = INT(p,q);
    while (BOOL(p,q))
      if (BOOL(p,q)) return BOOL(p,q);
      else ANY(p,q);
  }
  return BOOL(p);
}

int main () {
  if (BOOL()) ANY(); else ANY();
}

/*
BOOL(p,q): (p / q) * q == p



BOOL(p):   p == 1 || divides(2,p)
BOOL(p):   true

INT(p,q):  3
BOOL(p,q): q * q <= p
BOOL(p,q): divides(q,p)
BOOL(p,q): false
ANY(p,q):  q = q + 2
BOOL(p):   false


BOOL():    prime(641)
ANY():     printInt(641)
ANY():     printInt(0)
*/



/*
BOOL():    prime(641)
BOOL(p):   p == 1 || divides(2,p)
           true
           false
BOOL(p,q): (p / q) * q == p
           q * q <= p
           divides(q,p)
           false
INT(p,q):  3
ANY():     printInt(641)
           printInt(0)
ANY(p,q):  q = q + 2
*/
