bool i_want_a_bool(int not_a_bool) {
  return true;
}

int main () {
  int var = 1;

  {
    bool var = i_want_a_bool(var);
  }

  return 0;
}
