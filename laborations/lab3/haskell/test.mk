# Test compilation of the stub under various GHC versions

include ../../mk/test.mk

test-% : lab3.hs TypeChecker.hs Compiler.hs CMM/Lex.hs CMM/Par.hs
	@-rm CMM/*.hi CMM/*.o *.hi *.o 2> /dev/null
	ghc-$* -fno-code $<
	@touch $@

# Inherit parser generation from Makefile

include Makefile

# EOF
