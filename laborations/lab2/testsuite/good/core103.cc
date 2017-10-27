int main() {
  int j = 4;

  while (j < 6){ int i = 0 ; i++ ; printInt(i); j++; }
  if (j < 7) j++ ; else { j--; }

  printInt(j);
  return j;
}
