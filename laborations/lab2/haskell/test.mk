# Test compilation of the stub under various GHC versions.

include ../../mk/test.mk

test-% : lab2.hs TypeChecker.hs Interpreter.hs CMM/Lex.hs CMM/Par.hs
	@-rm CMM/*.hi CMM/*.o *.hi *.o 2> /dev/null
	ghc-$* -fno-code $<
	@touch $@

# Inherit parser generation from Makefile

include Makefile

# EOF
