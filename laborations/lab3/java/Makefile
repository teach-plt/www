# Makefile for PLT lab 3 in JAVA

# Variables for file lists
###########################################################################

# Edit to add new java source files, if needed!
LABSRC    = lab3.java TypeException.java TypeChecker.java Compiler.java \
            Runtime.java

# No need to edit these:
PARSERSRC = $(wildcard CPP/Absyn/*.java CPP/*.java)
PARSEROBJ = $(PARSERSRC:.java=.class)
LABOBJ    = $(LABSRC:.java=.class)
# Inner classes:
# LABINNOBJ = $(wildcard $(LABSRC:.java=$$*.class))

# Variables for the programming environment (edit as needed)
###########################################################################

# Name of generated .cup file for bnfc 2.8.1
CUPFILE = CPP/_cup.cup
# Older BNFC versions may need:
# CUPFILE = CPP/CPP.cup

JAVAC       = javac
JAVAC_FLAGS = -sourcepath .
JAVA        = java

# No need to edit these:
javac       = ${JAVAC} ${JAVAC_FLAGS}
java        = ${JAVA}  ${JAVA_FLAGS}

# Default rule
###########################################################################

# List of goals not corresponding to file names.
.PHONY : default all clean distclean vclean

# As the first goal is default goal, this goal needs to remain first.
default : lab3.class Runtime.class lab3

# Build and ship
all : default sdist

# Compiling the lab3 runner
###########################################################################

lab3 : lab3.hs
	ghc --make -cpp $< -o $@

# Rules for compiling lab3 classes (modify or add as needed)
###########################################################################

TypeChecker.class : TypeChecker.java TypeException.class CPP/Test.class
	$(javac) $<

Compiler.class : Compiler.java CPP/Test.class
	$(javac) $<

lab3.class : lab3.java TypeChecker.class Compiler.class CPP/Test.class
	$(javac) $<


# Rules for creating the parser
###########################################################################

# Create parser source via bnfc (dependency CPP.cf needs to be first).
# Patch CPP/Absyn/Exp.java
CPP/Yylex $(CUPFILE) CPP/Test.java : CPP.cf
	bnfc --java $<

# Create parser and move it to the correct location.
CPP/parser.java CPP/sym.java : $(CUPFILE)
	$(java) java_cup.Main -package CPP $<
	mv parser.java sym.java CPP/

# Create lexer.
CPP/Yylex.java : CPP/Yylex
	$(java) JLex.Main $<

# Compile lexer.
CPP/Yylex.class : CPP/Yylex.java CPP/sym.class
	$(javac) $<

# Create parser test.
CPP/Test.class : CPP/Test.java CPP/parser.class CPP/sym.class CPP/Yylex.class
	$(javac) $<


# Default rules
###########################################################################

%.class : %.java
	$(javac) $<


# Rules for shipping the solution
###########################################################################

sdist : lab3.tar.gz

tmpdir := $(shell mktemp -d)
lab3.tar.gz : CPP.cf Makefile lab3.hs $(LABSRC)
	mkdir $(tmpdir)/lab3
	cp $^ $(tmpdir)/lab3/
	tar -C $(tmpdir) -czhf $@ lab3

# Rules for cleaning generated files
###########################################################################

clean :
	-rm -f CPP/Absyn/*.class CPP/*.class
	-rm -f CPP.dvi CPP.aux CPP.log CPP.ps
	-rm -f $(LABOBJ)
	-rm -f lab3.hi lab3.o lab3.exe lab3
# Uncomment to also remove all .class files in current directory
#	-rm -f *.class


vclean : clean
	-rm -f $(PARSERSRC)
	-rm -f CPP/Absyn/*.bak CPP/*.bak
	-rmdir CPP/Absyn/
	-rm -f CPP.tex
	-rm -f CPP/Yylex $(CUPFILE)
	-rmdir -p CPP/

distclean : vclean
	-rm -f lab3.tar.gz

# Debugging the Makefile
###########################################################################

debug :
	echo $(LABINNOBJ)

# EOF
