int niam() {
  {
    return 99;
  }
  printInt(1);
}

int main() {
  {
    printInt(niam());
    return 0;
  }
  printInt(2);
}
