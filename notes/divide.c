int    idivii (int    x, int    y) { return (x / y); }
double ddivii (int    x, int    y) { return (x / y); }
double ddivid (double x, int    y) { return (x / y); }
double ddivdd (double x, double y) { return (x / y); }

int main() {
  printDouble(idivii(5,3));  //
  printDouble(ddivii(5,3));  //
  printDouble(ddivdi(5,3));  //
  printDouble(ddivdd(5,3));  //
}
