/* parity of positive integers by recursion */

int main () {
  printInt(ev(17)) ;
  return 0 ;
}

int ev (int y) {
  int e ;
  if (y > 0)
    e = ev (y-2) ;
  else
    if (y < 0)
      e = 0 ;
    else
      e = 1 ;
  return e ;
}
