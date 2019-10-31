int main ()
{
  int arg = readInt() ;
  int ret = 1 ;

  int i = 1 ;

  while (i < arg + 1) {
    ret = i * ret ;
    ++i ;
  }
  printInt(ret) ;

  return 0;
}
