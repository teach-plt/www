// Check that scope of then-statement does not leak into else-statement

int main () {
  if (false) int x = 1; else return x;
}
