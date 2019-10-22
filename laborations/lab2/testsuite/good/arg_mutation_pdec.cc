void cdec(int c) {
  c--;
}

int main () {
  int c = 1;
  cdec(--c);
  printInt(c);

  return 0;
}
