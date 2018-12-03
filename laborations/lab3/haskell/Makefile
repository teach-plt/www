# Makefile for PLT lab 3 in Haskell

## Variables
###########################################################################

# Files of solution.  Add/remove files as needed.  'lab2.hs'  has to be first!
files=lab3.hs CPP.cf Runtime.java Makefile Annotated.hs TypeChecker.hs Compiler.hs

###########################################################################

# List of goals not corresponding to file names.
.PHONY : default all sdist clean distclean

# lab3 must be first goal!
default : lab3

all : lab3 Runtime.class

# The extra dependency CPP/Test ensures that the parser is built from CPP.cf
lab3 : $(files) CPP/Test
	ghc --make lab3.hs -o lab3

# Rules to build the parser:

CPP/Test.hs CPP/Lex.x CPP/Layout.hs CPP/Par.y : CPP.cf
	bnfc --haskell -d $<

CPP/Par.hs: CPP/Par.y
	happy -gcai $<

CPP/Lex.hs: CPP/Lex.x
	alex -g $<

CPP/Test: CPP/Test.hs CPP/Par.hs CPP/Lex.hs
	ghc --make $< -o $@

%.class : %.java
	javac $<

# Rules for shipping the solution:

sdist: lab3.tar.gz

tmpdir := $(shell mktemp -d)
lab3.tar.gz: $(files)
	mkdir $(tmpdir)/lab3
	cp $^ $(tmpdir)/lab3/
	tar -C $(tmpdir) -czhf $@ lab3

# Rules for cleaning generated files:

clean:
	-rm -f CPP/*.log CPP/*.aux CPP/*.hi CPP/*.o CPP/*.dvi *.hi *.o

distclean: clean
	-rm -f CPP/Doc.* CPP/Lex.* CPP/Par.* CPP/Layout.* CPP/Skel.* CPP/Print.* CPP/Test.* CPP/Abs.* CPP/Test CPP/ErrM.* CPP/SharedString.* CPP/ComposOp.* CPP/CPP.dtd CPP/XML.* CPP/*.bak
	-rmdir -p CPP/
	-rm -f lab3.tar.gz lab3 Runtime.class

# EOF
