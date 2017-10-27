/* Test boolean operators */

int main () {
  int x = 4;
  if (3 <= x && 4 != 2 && true) {
    printBool(true);
  } else {

  }

  printBool(eq_bool(true,true) || dontCallMe(1));
  printBool(4 > 50 && dontCallMe(2));

  printBool(4 == x && eq_bool(true, false) && true);

  printBool(implies(false,false));
  printBool(implies(false,true));
  printBool(implies(true,false));
  printBool(implies(true,true));
  return 0 ;

}

bool dontCallMe(int x) {
  printInt(x);
  return true;
}

void printBool(bool b) {
  if (b) {
    printInt(1);
  } else {
    printInt(0);
 }
}

bool implies(bool x, bool y) {
  return not(x) || eq_bool(x,y);
}

bool not(bool x) {
  bool r ;
  if (x) r = false; else r = true;
  return r ;
}

bool eq_bool(bool x, bool y) {
  bool r ;
	if (x) 
		r = y;
	else 
		r = not(y);
	return r ;	
}
