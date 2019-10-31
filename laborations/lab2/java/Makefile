# Makefile for PLT lab 2 in JAVA

# Variables for file lists
###########################################################################

# Edit to add new java source files, if needed!
LABSRC    = TypeException.java TypeChecker.java Interpreter.java lab2.java

# No need to edit these:
PARSERSRC = $(wildcard CMM/Absyn/*.java CMM/*.java)
PARSEROBJ = $(PARSERSRC:.java=.class)
LABOBJ    = $(LABSRC:.java=.class)
# Inner classes:
# LABINNOBJ = $(wildcard $(LABSRC:.java=$$*.class))

# Variables for the programming environment (edit as needed)
###########################################################################

# Name of generated .cup file for bnfc 2.8.1
CUPFILE = CMM/_cup.cup
# Older BNFC versions may need:
# CUPFILE = CMM/CMM.cup

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
default : lab2.class lab2

# Build and ship
all : default sdist

# Compiling the lab2 runner
###########################################################################

lab2 : lab2.hs
	ghc --make -cpp $< -o $@

# Rules for compiling lab2 classes (modify or add as needed)
###########################################################################

TypeChecker.class : TypeChecker.java TypeException.class CMM/Test.class
	$(javac) $<

Interpreter.class : Interpreter.java CMM/Test.class
	$(javac) $<

lab2.class : lab2.java TypeChecker.class Interpreter.class CMM/Test.class
	$(javac) $<


# Rules for creating the parser
###########################################################################

# Create parser source via bnfc (dependency CMM.cf needs to be first).
CMM/Yylex $(CUPFILE) CMM/Test.java : CMM.cf
	bnfc --java $<

# Create parser and move it to the correct location.
CMM/parser.java CMM/sym.java : $(CUPFILE)
	$(java) java_cup.Main -package CMM $<
	mv parser.java sym.java CMM/

# Create lexer.
CMM/Yylex.java : CMM/Yylex
	$(java) JLex.Main $<

# Compile lexer.
CMM/Yylex.class : CMM/Yylex.java CMM/sym.class
	$(javac) $<

# Create parser test.
CMM/Test.class : CMM/Test.java CMM/parser.class CMM/sym.class CMM/Yylex.class
	$(javac) $<


# Default rules
###########################################################################

%.class : %.java
	$(javac) $<


# Rules for shipping the solution
###########################################################################

sdist : lab2.tar.gz

tmpdir := $(shell mktemp -d)
lab2.tar.gz : CMM.cf Makefile lab2.hs lab2.sh lab2.bat $(LABSRC)
	mkdir $(tmpdir)/lab2
	cp $^ $(tmpdir)/lab2/
	tar -C $(tmpdir) -czhf $@ lab2

# Rules for cleaning generated files
###########################################################################

clean :
	-rm -f CMM/Absyn/*.class CMM/*.class
	-rm -f CMM.dvi CMM.aux CMM.log CMM.ps
	-rm -f $(LABOBJ)
	-rm -f lab2.hi lab2.o lab2.exe lab2
# Uncomment to also remove all .class files in current directory
#	-rm -f *.class


vclean : clean
	-rm -f $(PARSERSRC)
	-rm -f CMM/Absyn/*.bak CMM/*.bak
	-rmdir CMM/Absyn/
	-rm -f CMM.tex
	-rm -f CMM/Yylex $(CUPFILE)
	-rmdir -p CMM/

distclean : vclean
	-rm -f lab2.tar.gz

# Debugging the Makefile
###########################################################################

debug :
	echo $(LABINNOBJ)

# EOF
