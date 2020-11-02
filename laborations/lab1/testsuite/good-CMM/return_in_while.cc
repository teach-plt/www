int rRrrrRrrrReturn() {
  int i = 0;

  while (i++ < 5) {
    return 71;
  }

  return 52;
}

int main() {
  printInt(rRrrrRrrrReturn());
  return 0;
}
