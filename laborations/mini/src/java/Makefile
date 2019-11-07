# Makefile for MiniJS parser, interpreter, and compiler implemented in Java.

# Programming Language Technology
# Chalmers DAT151, Gothenburg University DIT231

JAVAC=javac
JAVAC_FLAGS=-sourcepath .
JAVA=java
JAVA_FLAGS=
PARSER=${JAVA} ${JAVA_FLAGS} java_cup.Main
PARSER_FLAGS=-nopositions -expect 100
LEXER=${JAVA} ${JAVA_FLAGS} JLex.Main
LEXER_FLAGS=

.PHONY : test clean www index.html
.PRECIOUS : Runtime.class

test : Runtime.class Mini.class
	java Mini ../test/pi.js

Mini.class : Mini.java \
  MiniJS/Absyn/Program.class MiniJS/parser.class MiniJS/Yylex.class \
  Interpreter.class \
  Ann/Absyn/Program.class TypeChecker.class \
  Code/Absyn/Ins.class Compiler.class
	${JAVAC} ${JAVAC_FLAGS} $<

# Template

%.class : %.java
	${JAVAC} ${JAVAC_FLAGS} $<

# Create directory listing

www : index.html

index.html :
	tree -H '.' -L 2 --noreport --charset utf-8 > $@


# MiniJS

MiniJS/Absyn/Program.java : ../MiniJS.cf
	bnfc --java $<

MiniJS/Absyn/Program.class : MiniJS/Absyn/Program.java MiniJS/Absyn/Prg.java MiniJS/Absyn/Stm.java MiniJS/Absyn/SAssign.java MiniJS/Absyn/SPrint.java MiniJS/Absyn/ListStm.java MiniJS/Absyn/Exp.java MiniJS/Absyn/EVar.java MiniJS/Absyn/EInt.java MiniJS/Absyn/EDouble.java MiniJS/Absyn/ETimes.java MiniJS/Absyn/EDiv.java MiniJS/Absyn/EPlus.java MiniJS/Absyn/EMinus.java
	${JAVAC} ${JAVAC_FLAGS} $^

MiniJS/Yylex.java : MiniJS/Yylex
	${LEXER} ${LEXER_FLAGS} MiniJS/Yylex

MiniJS/parser.java MiniJS/sym.java : MiniJS/_cup.cup
	${PARSER} ${PARSER_FLAGS} MiniJS/_cup.cup
	mv parser.java sym.java MiniJS/

MiniJS/Yylex.class : MiniJS/Yylex.java MiniJS/parser.java MiniJS/sym.java

MiniJS/parser.class : MiniJS/parser.java MiniJS/sym.java

# Ann

Ann/Absyn/Program.java : ../Ann.cf
	bnfc --java $<

Ann/Absyn/Program.class : Ann/Absyn/Program.java Ann/Absyn/Prg.java Ann/Absyn/Stm.java Ann/Absyn/SAssign.java Ann/Absyn/SPrint.java Ann/Absyn/ListStm.java Ann/Absyn/Exp.java Ann/Absyn/EVar.java Ann/Absyn/EInt.java Ann/Absyn/EDouble.java
	${JAVAC} ${JAVAC_FLAGS} $^

Ann/Yylex.java : Ann/Yylex
	${LEXER} ${LEXER_FLAGS} Ann/Yylex

Ann/parser.java Ann/sym.java : Ann/_cup.cup
	${PARSER} ${PARSER_FLAGS} Ann/_cup.cup
	mv parser.java sym.java Ann/

Ann/Yylex.class : Ann/Yylex.java Ann/parser.java Ann/sym.java

Ann/parser.class : Ann/parser.java Ann/sym.java

# Code

Code/Absyn/Ins.java : ../Code.cf
	bnfc --java $<

Code/Absyn/Ins.class : Code/Absyn/Ins.java
	${JAVAC} ${JAVAC_FLAGS} $^

Code/Yylex.java : Code/Yylex
	${LEXER} ${LEXER_FLAGS} Code/Yylex

Code/parser.java Code/sym.java : Code/_cup.cup
	${PARSER} ${PARSER_FLAGS} Code/_cup.cup
	mv parser.java sym.java Code/

Code/Yylex.class : Code/Yylex.java Code/parser.java Code/sym.java

Code/parser.class : Code/parser.java Code/sym.java

# clean all generated and backup files

clean :
	rm -rf MiniJS Ann Code *.class *~

# EOF
