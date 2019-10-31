int main() {
  int x = 50;
  int y = x+++x--;
  printInt(y);
  printInt(x);
  printInt((x=10)+x+++x);
  printInt(x);
  return 0;
}
