// Andreas Abel, 2024-05-27
// Make sure a coercion is inserted in an initialization statement.

int main() {
  double d = oddInt();        // Needs coercion i2d here!
  printDouble(d);
  printDouble(d = oddInt());  // And here!
}

int oddInt() {
  return 5;
}
