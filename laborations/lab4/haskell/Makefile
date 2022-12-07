# Makefile for PLT lab 4 in Haskell

## Variables
###########################################################################

# Files of solution.  Add more files as needed.  'lab4.hs'  has to be first!
files=lab4.hs lab4.cabal Fun.cf Makefile Interpreter.hs

###########################################################################

# List of goals not corresponding to file names.
.PHONY : sdist clean distclean

# 'lab4' is default goal and needs to remain first goal in this file!
# The extra dependency Fun/Test ensures that the parser is built from Fun.cf.
# Building via 'cabal install' ensures that Haskell package dependencies
# listed in 'lab4.cabal' are picked up.
# Building via 'ghc lab4' is a bit faster, so if you only use packages
# shipped with GHC, you can use this as build command.

lab4 : $(files) Fun/Test
	cabal install --installdir=.
# If are only using the packages shipped with GHC, you can replace this by:
#	ghc lab4

# Rules to build the parser:

Fun/Test.hs Fun/Lex.x Fun/Par.y : Fun.cf
	bnfc -d $<

%.hs : %.y
	happy --ghc --coerce --array --info $<

%.hs : %.x
	alex --ghc $<

Fun/Test : Fun/Test.hs Fun/Par.hs Fun/Lex.hs
	ghc $@

# Rules for cleaning generated files:

clean :
	-rm -f Fun/*.log Fun/*.aux Fun/*.hi Fun/*.o Fun/*.dvi *.hi *.o

distclean : clean
	-rm -f Fun/Doc.* Fun/Lex.* Fun/Par.* Fun/Layout.* Fun/Skel.* Fun/Print.* Fun/Test.* Fun/Abs.* Fun/Test Fun/ErrM.* Fun/SharedString.* Fun/ComposOp.* Fun/Fun.dtd Fun/XML.* Fun/*.bak
	-rmdir -p Fun/
	-rm -f lab4

# EOF
