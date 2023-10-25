int g() {
  if (true) return 12;
  else return 11;
  return 10;
}

int main () {
  printInt(g());
  return 0;
}
