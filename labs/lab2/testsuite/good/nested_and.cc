// 2022-04-04 Attack potential exponential behavior in type checker.
// Some solutions check certain subexpressions twice (or even thrice)
// _per subexpression_, e.g. the left subexpression of conjunction.

int main () {
  bool b =
    true &&
    true &&
    true &&
    true &&
    true &&
    true &&
    true &&
    true &&
    true &&
    true &&
    true &&
    true &&
    true &&
    true &&
    true &&  //  0.9 sec
    true &&  //  2.1 sec
    true &&  //  5.4 sec
    true &&  // 15.9 sec
    true &&  // 46.8 sec
    false;
}
