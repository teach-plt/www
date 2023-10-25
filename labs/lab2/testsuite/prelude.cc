// Prelude for C-- language
//
// Can be added to test cases.  Add the pragma
//
//   #include "prelude.cc"
//
// to the top of the test case.  (If the prelude resides
// in the same directory as the test case.  Otherwise,
// adjust the path.)
//
// Then you should be able to compile and run the test case with
//
//   gcc <testcase>
//   a.out
//
// You can compare the output with the output of your interpreter/compiler.

#include <stdio.h>

// Print an integer and a newline to standard output.

void printInt (int i) {
  printf ("%d\n", i);
}

// Print a double and a newline to standard output.

void printDouble (double d) {
  printf ("%lf\n", d);
}

// Read an integer from standard input.

int readInt () {
  int result;
  scanf ("%d\n", &result);
  return result;
}

// Read a double from standard input.

double readDouble() {
  double result;
  scanf ("%lf\n", &result);
  return result;
}
