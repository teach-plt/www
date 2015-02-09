JAVAC = javac
JAVAC_FLAGS = -sourcepath .

JAVA = java

.PHONY: bnfc lab2 clean distclean vclean

all: bnfc lab2

lab2:
	${JAVAC} ${JAVAC_FLAGS} lab2.java
	chmod a+x lab2

bnfc:
	bnfc -java1.5 CPP.cf
	${JAVA} ${JAVA_FLAGS} JLex.Main CPP/Yylex
	${JAVA} ${JAVA_FLAGS} java_cup.Main -nopositions -expect 100 CPP/CPP.cup
	mv sym.java parser.java CPP

clean:
	 -rm -f CPP/Absyn/*.class CPP/*.class
	 -rm -f .dvi CPP.aux CPP.log CPP.ps  *.class

distclean: vclean

vclean: clean
	 -rm -f CPP/Absyn/*.java
	 -rmdir CPP/Absyn/
	 -rm -f CPP.tex CPP.dvi CPP.aux CPP.log CPP.ps 
	 -rm -f CPP/Yylex CPP/CPP.cup CPP/Yylex.java CPP/VisitSkel.java CPP/ComposVisitor.java CPP/AbstractVisitor.java CPP/FoldVisitor.java CPP/AllVisitor.java CPP/PrettyPrinter.java CPP/Skeleton.java CPP/Test.java CPP/sym.java CPP/parser.java CPP/*.class
	 -rmdir -p CPP/

