# Makefile for PLT lab 4 in JAVA

# Variables for file lists
###########################################################################

# Edit to add new java source files, if needed!
LABSRC    = Strategy.java Interpreter.java lab4.java

# No need to edit these:
LBNF      = fun
PARSERSRC = $(wildcard $(LBNF)/Absyn/*.java $(LBNF)/*.java)
PARSEROBJ = $(PARSERSRC:.java=.class)
LABOBJ    = $(LABSRC:.java=.class)
# Inner classes:
# LABINNOBJ = $(wildcard $(LABSRC:.java=$$*.class))

# Variables for the programming environment (edit as needed)
###########################################################################

# Name of generated .cup file for bnfc >= 2.8.1
CUPFILE = $(LBNF)/_cup.cup
# Older BNFC versions may need:
# CUPFILE = $(LBNF)/$(LBNF).cup

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
default : lab4.class lab4

# Build and ship
all : default sdist

# Compiling the lab4 runner
###########################################################################

lab4 : lab4.hs
	ghc --make -cpp $< -o $@

# Rules for compiling lab4 classes (modify or add as needed)
###########################################################################

Interpreter.class : Interpreter.java Strategy.class $(LBNF)/Test.class
	$(javac) $<

lab4.class : lab4.java Strategy.class Interpreter.class $(LBNF)/Test.class
	$(javac) $<


# Rules for creating the parser
###########################################################################

# Create parser source via bnfc (dependency $(LBNF).cf needs to be first).
$(LBNF)/Yylex $(CUPFILE) $(LBNF)/Test.java : $(LBNF).cf
	bnfc --java $<

# Create parser and move it to the correct location.
$(LBNF)/parser.java $(LBNF)/sym.java : $(CUPFILE)
	$(java) java_cup.Main -package $(LBNF) $<
	mv parser.java sym.java $(LBNF)/

# Create lexer.
$(LBNF)/Yylex.java : $(LBNF)/Yylex
	$(java) JLex.Main $<

# Compile lexer.
$(LBNF)/Yylex.class : $(LBNF)/Yylex.java $(LBNF)/sym.class
	$(javac) $<

# Create parser test.
$(LBNF)/Test.class : $(LBNF)/Test.java $(LBNF)/parser.class $(LBNF)/sym.class $(LBNF)/Yylex.class
	$(javac) $<


# Default rules
###########################################################################

%.class : %.java
	$(javac) $<


# Rules for shipping the solution
###########################################################################

sdist : lab4.tar.gz

tmpdir := $(shell mktemp -d)
lab4.tar.gz : $(LBNF).cf Makefile lab4.hs lab4.sh lab4.bat $(LABSRC)
	mkdir $(tmpdir)/lab4
	cp $^ $(tmpdir)/lab4/
	tar -C $(tmpdir) -czhf $@ lab4

# Rules for cleaning generated files
###########################################################################

clean :
	-rm -f $(LBNF)/Absyn/*.class $(LBNF)/*.class
	-rm -f $(LBNF).dvi $(LBNF).aux $(LBNF).log $(LBNF).ps
	-rm -f $(LABOBJ) Interpreter*.class
	-rm -f lab4.hi lab4.o lab4.exe lab4
# Uncomment to also remove all .class files in current directory
#	-rm -f *.class


vclean : clean
	-rm -f $(PARSERSRC)
	-rm -f $(LBNF)/Absyn/*.bak $(LBNF)/*.bak
	-rmdir $(LBNF)/Absyn/
	-rm -f $(LBNF).tex
	-rm -f $(LBNF)/Yylex $(CUPFILE)
	-rmdir -p $(LBNF)/

distclean : vclean
	-rm -f lab4.tar.gz

# Debugging the Makefile
###########################################################################

debug :
	echo $(LABINNOBJ)

# EOF
