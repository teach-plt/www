void printBool(bool b) {
  if (b)
    printInt(1);
  else
    printInt(0);
}

int main() {
  printBool(30 == 40);
  printBool(30 == 30);
  printBool(30 != 30);
  printBool(40 != 40);
  printBool(3 > 4);
  printBool(3 > 3);
  printBool(4 < 3);
  printBool(4 < 4);
  printBool(400 >= 300);
  printBool(400 >= 400);
  printBool(400 <= 300);
  printBool(400 <= 400);

  return 0;
}
