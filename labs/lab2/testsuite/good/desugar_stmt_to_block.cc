int f() {
  printInt(1);
  return 1;
}

int main() {
  if (true) int x = f(); else {}
  return 0;
}
