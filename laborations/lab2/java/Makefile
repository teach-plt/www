# Makefile for lab2 in java1.5

JAVAC = javac
JAVAC_FLAGS = -sourcepath .

JAVA = java

# Name of generated .cup file for bnfc 2.8.1
CUPFILE = CPP/_cup.cup
# WAS: CUPFILE = CPP/CPP.cup

.PHONY: bnfc clean distclean vclean

all: bnfc lab2.class

%.class: %.java
	${JAVAC} ${JAVAC_FLAGS} $<

bnfc:
	bnfc -java CPP.cf
	${JAVA} ${JAVA_FLAGS} JLex.Main CPP/Yylex
	${JAVA} ${JAVA_FLAGS} java_cup.Main -nopositions -expect 100 $(CUPFILE)
	mv sym.java parser.java CPP

clean:
	 -rm -f CPP/Absyn/*.class CPP/*.class
	 -rm -f .dvi CPP.aux CPP.log CPP.ps  *.class

distclean: vclean

vclean: clean
	 -rm -f CPP/Absyn/*.java
	 -rmdir CPP/Absyn/
	 -rm -f CPP.tex CPP.dvi CPP.aux CPP.log CPP.ps
	 -rm -f CPP/Yylex $(CUPFILE) CPP/Yylex.java CPP/VisitSkel.java CPP/ComposVisitor.java CPP/AbstractVisitor.java CPP/FoldVisitor.java CPP/AllVisitor.java CPP/PrettyPrinter.java CPP/Skeleton.java CPP/Test.java CPP/sym.java CPP/parser.java CPP/*.class
	 -rmdir -p CPP/

# EOF
