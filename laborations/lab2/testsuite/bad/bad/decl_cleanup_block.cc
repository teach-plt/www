// Check that scope of blocks is cleaned up

int main () {
  { int x = 1; }
  return x;
}
