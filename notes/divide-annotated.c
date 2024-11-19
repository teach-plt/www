int    idivii (int    x, int    y) { return         (x int./ y); }
double ddivii (int    x, int    y) { return (double)(x int./ y); }
double ddivid (double x, int    y) { return (x double./ (double)y); }
double ddivdd (double x, double y) { return (x double./ y); }

int main() {
  printDouble((double)idivii(5,3));           // 1.0
  printDouble(ddivii(5,3));                   // 1.0
  printDouble(ddivdi((double)5,3));           // 1.6666666666666667
  printDouble(ddivdd((double)5,(double)3));   // 1.6666666666666667
}
