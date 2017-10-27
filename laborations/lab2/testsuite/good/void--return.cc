void prant() {}

void prbnt() {
  return prant();
  printInt(0);
}

void prcnt() {
  return printInt(1);
  printInt(0);
}

void prdnt() {
  void c = prant();
  return c;
  printInt(0);
}

int main () {
  prant();
  prbnt();
  prcnt();
  prdnt();
  return 0;
}
