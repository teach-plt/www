# Test compilation of the stub under various GHC versions.

test : \
  test-7.10.3 \
  test-8.0.2  \
  test-8.2.2  \
  test-8.4.4  \
  test-8.6.4  \
  test-8.8.4  \
  test-8.10.2 \
# end test
# NB: 8.6.5 would be the latest 8.6, but I (Andreas Abel) do not have it installed.

test-% : lab2.hs TypeChecker.hs Interpreter.hs CMM/Lex.hs CMM/Par.hs
	ghc-$* -fno-code $<
	touch $@

# Inherit parser generation from Makefile

include Makefile

# EOF
