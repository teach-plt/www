# Makefile for lab2 in java1.5

JAVAC = javac
JAVAC_FLAGS = -sourcepath .

JAVA = java

# Name of generated .cup file for bnfc 2.8.1
CUPFILE = Fun/Fun.cup

grm = Fun
dir = $(grm)
bnfc_output = $(patsubst %,$(dir)/%,VisitSkel.java Absyn/EApp.java Absyn/Def.java Absyn/EAbs.java Absyn/EAdd.java Absyn/EVar.java Absyn/Main.java Absyn/DDef.java Absyn/EInt.java Absyn/EIf.java Absyn/Exp.java Absyn/DMain.java Absyn/ListIdent.java Absyn/ESub.java Absyn/Program.java Absyn/Prog.java Absyn/ELt.java Absyn/ListDef.java FoldVisitor.java Test.java ComposVisitor.java PrettyPrinter.java AbstractVisitor.java AllVisitor.java Yylex $(CUPFILE))
jlex_file = $(dir)/Yylex
cup_files = $(dir)/parser.java $(dir)/sym.java

.PHONY: lab4 clean distclean vclean

default: lab4

all: lab4

lab4.class : $(jlex_file).java $(cup_files) Interpreter.java lab4.java
	${JAVAC} ${JAVAC_FLAGS} lab4.java

lab4: lab4.class

$(bnfc_output) : $(grm).cf
	bnfc -java1.5 Fun.cf

$(jlex_file).java : $(jlex_file)
	${JAVA} ${JAVA_FLAGS} JLex.Main Fun/Yylex

$(cup_files) : $(CUPFILE)
	${JAVA} ${JAVA_FLAGS} java_cup.Main -nopositions -expect 100 $(CUPFILE)
	mv sym.java parser.java Fun

clean:
	 -rm -f Fun/Absyn/*.class Fun/*.class
	 -rm -f .dvi Fun.aux Fun.log Fun.ps  *.class

distclean: vclean

vclean: clean
	 -rm -f Fun/Absyn/*.java
	 -rmdir Fun/Absyn/
	 -rm -f Fun.tex Fun.dvi Fun.aux Fun.log Fun.ps
	 -rm -f Fun/Yylex $(CUPFILE) Fun/Yylex.java Fun/VisitSkel.java Fun/ComposVisitor.java Fun/AbstractVisitor.java Fun/FoldVisitor.java Fun/AllVisitor.java Fun/PrettyPrinter.java Fun/Skeleton.java Fun/Test.java Fun/sym.java Fun/parser.java Fun/*.class
	 -rmdir -p Fun/

# EOF
