int main() {
  double r ; // just to test blocks
  {
    int n = 10;
    int r = 1;
    while (n>0) {
      r = r * n;
      n = n - 1;
    }
    printInt(r);
  }
  printDouble(dfac(10.0));
  return 0 ;
}

double dfac(double n)
{
  double f ;
  if (n == 0.0)
    f = 1.0;
  else
    f = n * dfac(n-1.0);
  return f ;
}
