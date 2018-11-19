// Check that scope of while statement is cleaned up

int main () {
  while (false) int x = 1;
  return x;
}
