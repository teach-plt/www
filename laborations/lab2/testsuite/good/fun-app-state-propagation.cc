int Z(int z) { return z; }

int main() {
  int z = 0;
  Z(++z);
  printInt(z);
  Z(z = z + 1);
  printInt(z);
  return 0;
}
