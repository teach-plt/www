JAVAC = javac
JAVAC_FLAGS = -sourcepath .

JAVA = java

.PHONY: bnfc runmini compilemini clean distclean vclean

all: interpreter compiler

interpreter: bnfc runmini

compiler: bnfc compilemini

runmini:
	${JAVAC} ${JAVAC_FLAGS} runmini.java
	chmod a+x runmini

compilemini:
	${JAVAC} ${JAVAC_FLAGS} compilemini.java
	chmod a+x compilemini

bnfc:
	bnfc --java ../Mini.cf
	${JAVA} ${JAVA_FLAGS} JLex.Main Mini/Yylex
	${JAVA} ${JAVA_FLAGS} java_cup.Main -nopositions -expect 100 Mini/Mini.cup
	mv sym.java parser.java Mini

clean:
	 -rm -f Mini/Absyn/*.class Mini/*.class
	 -rm -f .dvi Mini.aux Mini.log Mini.ps  *.class

distclean: vclean

vclean: clean
	 -rm -f Mini/Absyn/*.java
	 -rmdir Mini/Absyn/
	 -rm -f Mini.tex Mini.dvi Mini.aux Mini.log Mini.ps
	 -rm -f Mini/Yylex Mini/Mini.cup Mini/Yylex.java Mini/VisitSkel.java Mini/ComposVisitor.java Mini/AbstractVisitor.java Mini/FoldVisitor.java Mini/AllVisitor.java Mini/PrettyPrinter.java Mini/Skeleton.java Mini/Test.java Mini/sym.java Mini/parser.java Mini/*.class
	 -rmdir -p Mini/
