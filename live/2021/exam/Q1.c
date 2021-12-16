#include <stdio.h>
#define  printInt3(e1,e2,e3) printf("%d %d %d\n",e1,e2,e3)

int main() {
  int x = 8;
  if (true) {
    printInt3 (x++, 10 + x++, x++ + 19);
  } else bool b = false;
}
