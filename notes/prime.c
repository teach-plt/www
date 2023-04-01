// Does q divide p?
bool divides (int q, int p) {
  return (p / q) * q == p;
}

// Is p prime?
bool prime (int p) {
  if (p == 1 || divides(2,p)) return false;
  else {
    int q = 3;
    while (q * q <= p)
      if (divides(q,p)) return false;
      else q = q + 2;
  }
  return true;
}

int main () {
  if (prime(641)) printInt(641); else printInt(0);
}
