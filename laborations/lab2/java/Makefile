# Makefile for PLT lab 2 in JAVA

# Variables for file lists
###########################################################################

# Edit to add new java source files, if needed!
LABSRC    = TypeException.java TypeChecker.java Interpreter.java lab2.java

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
default : lab2

# Build and ship
all : lab2 sdist

# Compiling the lab2 runner
###########################################################################

lab2 : lab2.hs lab2.class
	ghc --make -cpp $< -o $@

# Rules for compiling lab2 classes (modify or add as needed)
###########################################################################

TypeChecker.class : TypeChecker.java TypeException.class CPP/Test.class
	$(javac) $<

Interpreter.class : Interpreter.java CPP/Test.class
	$(javac) $<

lab2.class : lab2.java TypeChecker.class Interpreter.class CPP/Test.class
	$(javac) $<


# Rules for creating the parser
###########################################################################

# Create parser source via bnfc (dependency CPP.cf needs to be first).
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

sdist : lab2.tar.gz

tmpdir := $(shell mktemp -d)
lab2.tar.gz : CPP.cf Makefile $(LABSRC)
	mkdir $(tmpdir)/lab2
	cp $^ $(tmpdir)/lab2/
	tar -C $(tmpdir) -czhf $@ lab2

# Rules for cleaning generated files
###########################################################################

clean :
	-rm -f CPP/Absyn/*.class CPP/*.class
	-rm -f CPP.dvi CPP.aux CPP.log CPP.ps
	-rm -f $(LABOBJ)
	-rm -f lab.hi lab.o lab2.exe lab2
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
	-rm -f lab2.tar.gz

# Debugging the Makefile
###########################################################################

debug :
	echo $(LABINNOBJ)

# EOF
