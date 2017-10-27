int main() {
  printBool(test(0-1) && test(0));
  printBool(test(0-2) || test(1));
  printBool(test(3) && test(0-5) && true);
  printBool(test(3) || test(0-5) && true);
  printBool(true);
  printBool(false);
  return 0 ;
}

void printBool(bool b) {
  if (b) {
    printInt(1);
  } else {
    printInt(0);
 }
}

bool test(int i) {
  return i > 0;
}
