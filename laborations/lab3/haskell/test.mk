# Test compilation of the stub under various GHC versions.

test : \
  test-7.10.3 \
  test-8.0.2  \
  test-8.2.2  \
  test-8.4.4  \
  test-8.6.5  \
  test-8.8.4  \
  test-8.10.7 \
  test-9.0.1  \
# end test

test-% : lab3.hs TypeChecker.hs Compiler.hs CMM/Lex.hs CMM/Par.hs
	ghc-$* -fno-code $<
	touch $@

# Inherit parser generation from Makefile

include Makefile

# EOF
