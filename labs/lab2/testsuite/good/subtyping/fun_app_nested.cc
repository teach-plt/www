// Andreas Abel, 2024-05-27
// Nested function calls to expose limitations in subtyping implementations.

int main() {
  printDouble(half(oddInt()));
}

int oddInt() {
  return 5;
}

double half(double x) {
  return x / 2;
}
