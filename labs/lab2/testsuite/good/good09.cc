int main () 
{
  int i = readInt() ; // 5

  printInt(i) ;   //5
  printInt(i++) ; //5
  printInt(i) ;   //6
  printInt(++i) ; //7
  printInt(i) ;   //7

  return 0;
}
