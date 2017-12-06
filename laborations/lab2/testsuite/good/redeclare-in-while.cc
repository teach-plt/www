// Extra scope for while body needed.

int main() {
  int n = 0;
  while (n++ < 10) int n = 100;
  printInt(n);  // Should print 11
  return 0;
}
