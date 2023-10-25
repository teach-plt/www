// Check that scope of if statement is cleaned up

int main () {
  if (false) {} else int x = 0;
  return x;
}
