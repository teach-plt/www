void f2 () {
  struct s1_ {};
  struct s2_ { int y; };
  struct s3_ { std::vector<bool>::value_type y; };
  struct s4_ { foo y; std::string asd; };

  typedef a::b::c<d>::q foo;
}
