# Makefile for PLT lab 2 in Haskell

## Variables
###########################################################################

# Files of solution.  Add more files as needed.  'lab2.hs'  has to be first!
files=lab2.hs CPP.cf Makefile TypeChecker.hs Interpreter.hs

###########################################################################

# List of goals not corresponding to file names.
.PHONY: sdist clean distclean

# 'lab2' is default goal and needs to remain first goal in this file!
# The extra dependency CPP/Test ensures that the parser is built from CPP.cf

lab2: $(files) CPP/Test
	ghc --make lab2.hs -o lab2

# Rules to build the parser:

CPP/Test.hs CPP/Lex.x CPP/Layout.hs CPP/Par.y : CPP.cf
	bnfc --haskell -d $<

CPP/Par.hs: CPP/Par.y
	happy -gcai $<

CPP/Lex.hs: CPP/Lex.x
	alex -g $<

CPP/Test: CPP/Test.hs CPP/Par.hs CPP/Lex.hs
	ghc --make $< -o $@

# Rules for shipping the solution:

sdist: lab2.tar.gz

tmpdir := $(shell mktemp -d)
lab2.tar.gz: $(files)
	mkdir $(tmpdir)/lab2
	cp $^ $(tmpdir)/lab2/
	tar -C $(tmpdir) -czhf $@ lab2

# Rules for cleaning generated files:

clean:
	-rm -f CPP/*.log CPP/*.aux CPP/*.hi CPP/*.o CPP/*.dvi *.hi *.o

distclean: clean
	-rm -f CPP/Doc.* CPP/Lex.* CPP/Par.* CPP/Layout.* CPP/Skel.* CPP/Print.* CPP/Test.* CPP/Abs.* CPP/Test CPP/ErrM.* CPP/SharedString.* CPP/ComposOp.* CPP/CPP.dtd CPP/XML.* CPP/*.bak
	-rmdir -p CPP/
	-rm -f lab2.tar.gz lab2

# EOF
