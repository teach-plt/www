int main() {
  if (true)
    if (false)
      printInt(2);
    else
      printInt(1);
  else
    if (true)
      printInt(3);
    else
      printInt(0);
  return 0;
}
