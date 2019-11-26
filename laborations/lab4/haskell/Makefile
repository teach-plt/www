# Makefile for PLT lab 4 in Haskell

## Variables
###########################################################################

# Files of solution.  Add more files as needed.  'lab4.hs'  has to be first!
files=lab4.hs Fun.cf Makefile Interpreter.hs

###########################################################################

# List of goals not corresponding to file names.
.PHONY : sdist clean distclean

# 'lab4' is default goal and needs to remain first goal in this file!
# The extra dependency Fun/Test ensures that the parser is built from Fun.cf

lab4 : $(files) Fun/Test
	ghc --make lab4.hs -o lab4

# Rules to build the parser:

Fun/Test.hs Fun/Lex.x Fun/Par.y : Fun.cf
	bnfc --ghc -d $<

%.hs : %.y
	happy -gcai $<
#	happy --ghc --coerce --array --info $<

%.hs : %.x
	alex --ghc $<

Fun/Test : Fun/Test.hs Fun/Par.hs Fun/Lex.hs
	ghc --make $< -o $@

# Rules for shipping the solution:

sdist : lab4.tar.gz

tmpdir := $(shell mktemp -d)
lab4.tar.gz : $(files)
	mkdir $(tmpdir)/lab4
	cp $^ $(tmpdir)/lab4/
	tar -C $(tmpdir) -czhf $@ lab4

# Rules for cleaning generated files:

clean :
	-rm -f Fun/*.log Fun/*.aux Fun/*.hi Fun/*.o Fun/*.dvi *.hi *.o

distclean : clean
	-rm -f Fun/Doc.* Fun/Lex.* Fun/Par.* Fun/Layout.* Fun/Skel.* Fun/Print.* Fun/Test.* Fun/Abs.* Fun/Test Fun/ErrM.* Fun/SharedString.* Fun/ComposOp.* Fun/Fun.dtd Fun/XML.* Fun/*.bak
	-rmdir -p Fun/
	-rm -f lab4.tar.gz lab4

# EOF
