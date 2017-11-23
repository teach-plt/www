// Test whether < and > on doubles work.

// The description of the double comparison JVM instructions dcmpg and dcmpl
// is wrong in the PLT book of 2012.  If one follows the description,
// this test may break.

int main() {
  double big   = 1.5;
  double small = 0.5;
  if (big > small) printInt(1); else printInt(0);  // print 1
  if (small < big) printInt(1); else printInt(0);  // print 1
}
