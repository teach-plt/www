/* Test arithmetic and comparisons. */

void printBool(bool b) {
  if (b) {
    printInt(1);
  } else {
    printInt(0);
  }
}

int main() {
    double z = 9.3;
    double w = 5.1;
    printBool(z+w > z-w);
    printBool(z/w <= z*w);
    return 0;
}
