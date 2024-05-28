// Andreas Abel, 2024-05-27
// Blocks of different size to expose bugs in the `.limit local` counts.

int main () {
  int a = 0;
  {
    int x = 1;
    int y = 2;
    int z = 3;
    a = x + y + z;
  }
  {
    int u = 4;
    int v = 5;
    a = a + u + v;
  }
  {
    int w = 6;
    a = a + w;
  }
  printInt(a);
}
