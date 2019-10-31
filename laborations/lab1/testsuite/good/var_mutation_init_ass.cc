int main() {
  int a;
  int b = a = 0;

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
