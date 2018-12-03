# Makefile for PLT lab 2 in JAVA

## Variables
###########################################################################

JAVAC = javac
JAVAC_FLAGS = -sourcepath .

JAVA = java

# Name of generated .cup file for bnfc 2.8.1
CUPFILE = CPP/_cup.cup
# Older BNFC versions may need:
# CUPFILE = CPP/CPP.cup

PARSERSRC = $(wildcard CPP/Absyn/*.java CPP/*.java)
PARSEROBJ = $(PARSERSRC:.java=.class)
LABSRC    = TypeException.class TypeChecker.class Interpreter.class lab2.class
LABOBJ    = $(LABSRC:.java=.class)

###########################################################################

# List of goals not corresponding to file names.
.PHONY: bnfc sdist clean distclean vclean

# As first goal is default goal, this goal needs to remain first.
all: bnfc lab2 $(LABOBJ)

lab2: lab2.hs
	ghc --make -cpp $< -o $@

%.class: %.java
	${JAVAC} ${JAVAC_FLAGS} $<

bnfc:
	bnfc -java CPP.cf
	${JAVA} ${JAVA_FLAGS} JLex.Main CPP/Yylex
	${JAVA} ${JAVA_FLAGS} java_cup.Main -nopositions -expect 100 $(CUPFILE)
	mv sym.java parser.java CPP

# Rules for shipping the solution:

sdist: lab2.tar.gz

tmpdir := $(shell mktemp -d)
lab2.tar.gz: CPP.cf Makefile lab2.hs lab2.java Interpreter.java TypeChecker.java TypeException.java
	# The --transform option is only supported by GNU tar.
	# tar --transform='s,^,lab2/,' -czhf $@ $^
	mkdir $(tmpdir)/lab2
	cp $^ $(tmpdir)/lab2/
	tar -C $(tmpdir) -czhf $@ lab2

# Rules for cleaning generated files:

clean:
	 -rm -f CPP/Absyn/*.class CPP/*.class
	 -rm -f .dvi CPP.aux CPP.log CPP.ps
	 -rm -f $(LABOBJ)
	 -rm -f lab2.hi lab2.o lab2.exe lab2

vclean: clean
	 -rm -f $(PARSERSRC)
	 -rmdir CPP/Absyn/
	 -rm -f CPP.tex CPP.dvi CPP.aux CPP.log CPP.ps
	 -rm -f CPP/Yylex $(CUPFILE)
	 -rmdir -p CPP/

distclean: vclean
	-rm -f lab2.tar.gz

# EOF
