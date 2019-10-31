// Extra scope for while body needed.

int main() {
  int n = 0;
  while (n++ < 10) int m = 100;
  int m = n;
  printInt(m);  // Should print 11
  return 0;
}
