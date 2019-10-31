int id(int x) { return x; }

int main() {
  int z = 0;

  // Function calls
  id(++z);
  printInt(z);
  id(z = z + 1);
  printInt(z);

  // Expressions
  z++ + z++;
  printInt(z);

  z++ == z++;
  printInt(z);

  // Conditions
  if (++z == 7) { printInt(z); } else { }
  printInt(z);

  if (z++ == 8) { } else { printInt(z); }
  printInt(z);

  while (++z == 9) { printInt(z); }
  printInt(z);

  while (z++ == 11) { }
  printInt(z);

  return 0;
}
