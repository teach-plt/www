int main () {
  // const const int c = 123;
     // warning: duplicate 'const' declaration specifier [-Wduplicate-decl-specifier]
  typedef const int my_int;  // Legal C++, but not included in PLT 2020 lab 1
  const my_int z = 456;      // C++: no warning.
  my_int x = 1;
  x = 2; // error: cannot assign to variable 'x' with const-qualified type 'my_int' (aka 'const int')
}
