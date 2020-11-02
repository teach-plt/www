int snd (int x, int y) {
  return y;
}

int main () {
  int x = 0;
  int r = snd(1,x);
  printInt(r); // Should print 0
  return r;
}
