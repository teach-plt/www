int main() {
  int a = 0 - 1;
  int b; b = ++a;

  ++b;
  printInt(a);

  b++;
  printInt(a);

  --b;
  printInt(a);

  b--;
  printInt(a);

  b = 832;
  printInt(a);

  return 0;
}
