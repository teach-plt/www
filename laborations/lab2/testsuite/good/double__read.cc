// 2022-12-08 Andreas Abel
// Simple test to cover readDouble.
//
// N.B.: Because of locale issues, it is not easy to read "proper" floats
// like "3.14". On some systems this errors out because "3,14" would be
// the floating point syntax.

int main() {
  printDouble(readDouble());
  printDouble(readDouble());
  printDouble(readDouble());
}
