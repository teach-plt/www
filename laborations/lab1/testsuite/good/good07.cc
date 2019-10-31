int main ()
{
  int x = readInt () ;

  int d = x/2 ;

  while (d > 1) {
    if (d * (x/d) == x)
      printInt(d) ;
    else
      {}

    d-- ;
  }

  return 0;
}
