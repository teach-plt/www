int main() {
  printInt(f(45));
  printInt(f(450));
  return 0;
}

int f(int x) {
  int y ;
  if (x < 100) {
    int x = 91;
    y = x;
  } else {
    y = x;
  }
  return y ;
}
