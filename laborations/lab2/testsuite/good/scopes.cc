
int f() {
  int n = 2;
  if (n < 3) {
    int n = 3;
    return n;
  } else { }
  return n;
}

int main() {
  int n = 1;
  printInt(n);
  printInt(f());
  printInt(n);
  return 0;
}
