#include <stdio.h>

const int f (int x) { return x; }

int main() {
  int y;
  const int& x = y;  // Valid C++ (needs initialization). `const` has no effect.
  y = 3;
  printf("%d", x);   // Prints 3
  return f(x);
}
