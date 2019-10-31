/* NOTE: this tests whether identifier tokens are allowed
         to contain single quotes. This is not allowed in C++, 
         and should not be allowed by your grammar. */
int foo() {
	int x' = 4;
}
