# Makefile for PLT lab 2 in Haskell

## Variables
###########################################################################

# Files of solution.  Add more files as needed.  'lab2.hs'  has to be first!
files=lab2.hs CMM.cf Makefile TypeChecker.hs Interpreter.hs

###########################################################################

# List of goals not corresponding to file names.
.PHONY : sdist clean distclean

# 'lab2' is default goal and needs to remain first goal in this file!
# The extra dependency CMM/Test ensures that the parser is built from CMM.cf

lab2 : $(files) CMM/Test
	ghc --make lab2.hs -o lab2

# Rules to build the parser:

CMM/Test.hs CMM/Lex.x CMM/Par.y : CMM.cf
	bnfc --haskell -d $<

%.hs : %.y
	happy -gcai $<
#	happy --ghc --coerce --array --info $<

%.hs : %.x
	alex --ghc $<

CMM/Test : CMM/Test.hs CMM/Par.hs CMM/Lex.hs
	ghc --make $< -o $@

# Rules for shipping the solution:

sdist : lab2.tar.gz

tmpdir := $(shell mktemp -d)
lab2.tar.gz : $(files)
	mkdir $(tmpdir)/lab2
	cp $^ $(tmpdir)/lab2/
	tar -C $(tmpdir) -czhf $@ lab2

# Rules for cleaning generated files:

clean :
	-rm -f CMM/*.log CMM/*.aux CMM/*.hi CMM/*.o CMM/*.dvi *.hi *.o

distclean : clean
	-rm -f CMM/Doc.* CMM/Lex.* CMM/Par.* CMM/Layout.* CMM/Skel.* CMM/Print.* CMM/Test.* CMM/Abs.* CMM/Test CMM/ErrM.* CMM/SharedString.* CMM/ComposOp.* CMM/CMM.dtd CMM/XML.* CMM/*.bak
	-rmdir -p CMM/
	-rm -f lab2.tar.gz lab2

# EOF
