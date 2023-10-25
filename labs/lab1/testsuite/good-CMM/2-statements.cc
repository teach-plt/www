// This test file contains function definitions with statements.
// Expressions are restricted to literals.

bool f () {
  int x = 3;
  double z0, z1;
  while (true) {
    bool z = true;
    if (false) int y = 0;
    else {
      double pi = 3.14159;
      return false;
    }
    {
      while (false) int a, b, c;
      while (false)
        while (true)
          while (false)
            if (true)
              if (false) {}
              else if (false) return true;
              else {}
            else while (false) return false;
      {}
      {{}}
      {{{{ int foo; }}}}
    }
  }
}

/* The main function is not very impressive.
   This is a pointless comment.

   /* no nesting of block comments!

 */

int main () {
  return 0;
}
