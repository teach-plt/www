int main() {
  int x = 5;

  if (++x == x++)
    printInt(x);
  else
    printInt(0);

  return 0;
}
