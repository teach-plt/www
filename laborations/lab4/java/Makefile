# Makefile for lab2 in java1.5

JAVAC = javac
JAVAC_FLAGS = -sourcepath .

JAVA = java

# Name of generated .cup file for bnfc 2.8 master
CUPFILE = Fun/_cup.cup

test_file = ./lab4

.PHONY: bnfc lab4 clean distclean vclean

default: bnfc lab4

all: bnfc lab4 test

test : $(test_file)
	-$(test_file) test/good1.hs
	-$(test_file) -n test/good1.hs
	-$(test_file) -n test/good2.hs
	-$(test_file) test/good3.hs
	-$(test_file) -n test/good3.hs
	-$(test_file) test/good4.hs
#	-$(test_file) -n test/good4.hs
	-$(test_file) -n test/good5.hs
	-$(test_file) test/good6.hs
#	-$(test_file) -n test/good6.hs
	-$(test_file) test/good7.hs
#	-$(test_file) -n test/good7.hs
	-$(test_file) test/good8.hs
	-$(test_file) -n test/good8.hs
	-$(test_file) test/good9.hs
	-$(test_file) -n test/good9.hs
	-! $(test_file) test/bad1.hs
	-! $(test_file) test/bad2.hs
	-! $(test_file) test/bad3.hs
	-! $(test_file) test/bad4.hs
	-! $(test_file) test/bad5.hs
	-! $(test_file) test/bad6.hs

lab4:
	${JAVAC} ${JAVAC_FLAGS} lab4.java
	chmod a+x lab4

bnfc:
	bnfc -java1.5 Fun.cf
	${JAVA} ${JAVA_FLAGS} JLex.Main Fun/Yylex
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
