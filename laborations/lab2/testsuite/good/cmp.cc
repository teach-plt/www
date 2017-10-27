int main() {
  printBool(40 == 40);
  printBool(40 != 40);
  printBool(3 > 4);
  printBool(4 < 3);
  printBool(400 >= 300);
  printBool(400 <= 300);
}

void printBool(bool b) {
  if (b)
    printInt(1);
  else
    printInt(0);
}
