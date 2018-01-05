int order(int a, int b) { return 0; }

int printIntInt(int i) {
  printInt(i);
  return i;
}

bool printIntBool(int i) {
  printInt(i);
  return true;
}

int main () {
  order(printIntInt(0), printIntInt(1));
  printIntInt(2) + printIntInt(3);
  printIntInt(4) == printIntInt(5);
  printIntInt(6) * printIntInt(7);
  printIntInt(8) / printIntInt(9);
  printIntInt(10) - printIntInt(11);
  printIntInt(12) < printIntInt(13);
  printIntInt(14) > printIntInt(15);
  printIntInt(16) <= printIntInt(17);
  printIntInt(18) >= printIntInt(19);
  printIntInt(20) != printIntInt(21);
  printIntBool(22) && printIntBool(23);
  printIntBool(24) || printIntBool(25);
  return 0;
}
