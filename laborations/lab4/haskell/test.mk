# Test compilation of the stub under various GHC versions.

include ../../mk/test.mk

test-% : lab4.hs Interpreter.hs Fun/Lex.hs Fun/Par.hs
	@-rm Fun/*.hi Fun/*.o *.hi *.o 2> /dev/null
	ghc-$* -fno-code $<
	@touch $@

# Inherit parser generation from Makefile

include Makefile

# EOF
