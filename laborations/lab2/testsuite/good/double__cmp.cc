// Lab 3 note:
// The description of the double comparison JVM instructions dcmpg and dcmpl
// is wrong in the PLT book of 2012. If one follows the description,
// this test may break.

void printBool(bool b) {
  if (b)
    printInt(1);
  else
    printInt(0);
}

int main() {
  double big = 1.5;
  double small = 0.5;

  printBool(big == big);
  printBool(big != big);

  printBool(big > small);
  printBool(big > big);

  printBool(small >= big);
  printBool(small >= small);

  printBool(small < big);
  printBool(big < big);

  printBool(small <= big);
  printBool(small <= small);

  return 0;
}
