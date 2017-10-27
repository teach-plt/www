int main () {
  int x = 0;

  if (true)
    { int x = 1; }
  else
    {}
  printInt(x);

  if (true) {
    int x;
    x = 2;
  }
  else
    {}
  printInt(x);

  if (false)
    {}
  else
    { int x = 3; }
  printInt(x);

  if (false)
    {}
  else {
    int x;
    x = 4;
  }
  printInt(x);

  {
    int i = 0;
    while (i++ < 1)
      { int x = 5; }
    printInt(x);
  }

  {
    int i = 0;
    while (i++ < 1) {
      int x;
      x = 6;
    }
    printInt(x);
  }

  {
    int x = 7;
  }
  printInt(x);

  {
    int x;
    x = 8;
  }
  printInt(x);

  return 0;
}
