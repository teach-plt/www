// Does p divide q?
bool divides (int p, int q) {
  return (q / p) * p == q;
}

// Is p prime?
bool prime (int p) {
  if (p <= 2) return p == 2;
  else {
    int q = 3;
    while (q * q <= p)
      if (divides(q,p)) return false;
      else q = q + 2;
  }
  return true;
}
