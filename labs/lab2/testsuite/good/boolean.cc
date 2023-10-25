// Simple test for shortcut boolean operations

bool ff () {
  printInt(0);
  return false;
}
bool tt () {
  printInt(1);
  return true;
}

int main () {
  bool t = true;
  bool f = false;
  bool silent1 = f && ff();  // nothing
  bool silent2 = t || ff();  // nothing
  bool noisy1  = t && tt();  // 1
  bool noisy2  = f || tt();  // 1
  return 0;
}
