# Makefile for PLT lab 3 in JAVA

# Variables for file lists
###########################################################################

# Edit to add new java source files, if needed!
LABSRC    = lab3.java TypeException.java TypeChecker.java Compiler.java \
            Runtime.java

# No need to edit these:
PARSERSRC = $(wildcard cmm/Absyn/*.java cmm/*.java)
PARSEROBJ = $(PARSERSRC:.java=.class)
LABOBJ    = $(LABSRC:.java=.class)
# Inner classes:
# LABINNOBJ = $(wildcard $(LABSRC:.java=$$*.class))

# Variables for the programming environment (edit as needed)
###########################################################################

# Name of generated .cup file for bnfc ≥ 2.8.1
CUPFILE = cmm/_cup.cup
# Older BNFC versions may need:
# CUPFILE = cmm/cmm.cup

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

TypeChecker.class : TypeChecker.java TypeException.class cmm/Test.class
	$(javac) $<

Compiler.class : Compiler.java cmm/Test.class
	$(javac) $<

lab3.class : lab3.java TypeChecker.class Compiler.class cmm/Test.class
	$(javac) $<


# Rules for creating the parser
###########################################################################

# Create parser source via bnfc (dependency cmm.cf needs to be first).
# Patch cmm/Absyn/Exp.java
cmm/Yylex $(CUPFILE) cmm/Test.java : cmm.cf
	bnfc --java $<

# Create parser and move it to the correct location.
cmm/parser.java cmm/sym.java : $(CUPFILE)
	$(java) java_cup.Main -package cmm $<
	mv parser.java sym.java cmm/

# Create lexer.
cmm/Yylex.java : cmm/Yylex
	$(java) JLex.Main $<

# Compile lexer.
cmm/Yylex.class : cmm/Yylex.java cmm/sym.class
	$(javac) $<

# Create parser test.
cmm/Test.class : cmm/Test.java cmm/parser.class cmm/sym.class cmm/Yylex.class
	$(javac) $<


# Default rules
###########################################################################

%.class : %.java
	$(javac) $<


# Rules for shipping the solution
###########################################################################

sdist : lab3.tar.gz

tmpdir := $(shell mktemp -d)
lab3.tar.gz : cmm.cf Makefile lab3.hs lab3.sh lab3.bat jasmin.jar $(LABSRC)
	mkdir $(tmpdir)/lab3
	cp $^ $(tmpdir)/lab3/
	tar -C $(tmpdir) -czhf $@ lab3

# Rules for cleaning generated files
###########################################################################

clean :
	-rm -f cmm/Absyn/*.class cmm/*.class
	-rm -f cmm.dvi cmm.aux cmm.log cmm.ps
	-rm -f $(LABOBJ)
	-rm -f lab3.hi lab3.o lab3.exe lab3
# Uncomment to also remove all .class files in current directory
#	-rm -f *.class


vclean : clean
	-rm -f $(PARSERSRC)
	-rm -f cmm/Absyn/*.bak cmm/*.bak
	-rmdir cmm/Absyn/
	-rm -f cmm.tex
	-rm -f cmm/Yylex $(CUPFILE)
	-rmdir -p cmm/

distclean : vclean
	-rm -f lab3.tar.gz

# Debugging the Makefile
###########################################################################

debug :
	echo $(LABINNOBJ)

# EOF
