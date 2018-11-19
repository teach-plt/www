void printBool(bool b) {
  if (b)
    printInt(1);
  else
    printInt(0);
}

int main() {
  printBool(true == true);
  printBool(true == false);
  printBool(true != false);

  return 0;
}
