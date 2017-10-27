int order(int a, int b) { return 0; }

int printIntInt(int i) {
  printInt(i);
  return i;
}

int main () {
  order(printIntInt(0), printIntInt(1));
  printIntInt(2) + printIntInt(3);
  printIntInt(4) == printIntInt(5);
  return 0;
}
