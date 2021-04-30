/* We don't do structs anymore.  Nor template instantiations.
But this tests for non-nesting block comments.
/*
struct s1 {};
struct s2 { int y; };
struct s3 { std::vector<bool>::value_type y; };
struct s4 { foo y; std::string asd; };
*/
