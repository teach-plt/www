// Check that scope of if statement is cleaned up

int main () {
  if (false) int x = 0; else {}
  return x;
}
