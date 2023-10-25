# Makefile for Haskell implementation of MiniJS

# List of goals not corresponding to file names.

.PHONY : all clean distclean www index.html

# Default goal.

all : mini test

test : mini
	./mini ../test/pi.js
#	./mini ../test/ex1.js
#	./mini ../test/arith.js
#	./mini ../test/unbound.js


# Create directory listing

www : index.html

index.html :
	tree -H '.' -L 2 --noreport --charset utf-8 > $@

# MiniJS implementation

mini : Main.hs Interpreter.hs Types.hs TypeChecker.hs Ann.hs AnnTypeChecker.hs JVM.hs Compiler.hs \
  MiniJS/Abs.hs MiniJS/Lex.x MiniJS/Print.hs MiniJS/Par.y \
  Code/Abs.hs Code/Print.hs \
  Runtime.class
	ghc --make -o $@ $<

%.class : %.java
	javac $<

# Rules for building the parser.

MiniJS/ErrM.hs MiniJS/Lex.x MiniJS/Print.hs MiniJS/Par.y MiniJS/Test.hs : MiniJS.cf
	bnfc --haskell -d MiniJS.cf

%.hs : %.y
	happy --ghc --coerce --array --info $<

%.hs : %.x
	alex --ghc $<

MiniJS/Test : MiniJS/Test.hs MiniJS/ErrM.hs MiniJS/Lex.hs MiniJS/Par.hs MiniJS/Print.hs
	ghc --make $< -o $@

# Rules for building the parser.

Code/ErrM.hs Code/Lex.x Code/Print.hs Code/Par.y Code/Test.hs : ../Code.cf
	bnfc --haskell -d $<

Code/Test : Code/Test.hs Code/ErrM.hs Code/Lex.hs Code/Par.hs Code/Print.hs
	ghc --make $< -o $@

# Rules for cleaning generated files.

clean :
	-rm -f MiniJS/*.hi MiniJS/*.o MiniJS/*.log MiniJS/*.aux MiniJS/*.dvi
	-rm -f Code/*.hi Code/*.o Code/*.log Code/*.aux Code/*.dvi

distclean : clean
	-rm -f MiniJS/Abs.hs MiniJS/Abs.hs.bak MiniJS/ComposOp.hs MiniJS/ComposOp.hs.bak MiniJS/Doc.txt MiniJS/Doc.txt.bak MiniJS/ErrM.hs MiniJS/ErrM.hs.bak MiniJS/Layout.hs MiniJS/Layout.hs.bak MiniJS/Lex.x MiniJS/Lex.x.bak MiniJS/Par.y MiniJS/Par.y.bak MiniJS/Print.hs MiniJS/Print.hs.bak MiniJS/SharedString.hs MiniJS/SharedString.hs.bak MiniJS/Skel.hs MiniJS/Skel.hs.bak MiniJS/Test.hs MiniJS/Test.hs.bak MiniJS/XML.hs MiniJS/XML.hs.bak MiniJS/AST.agda MiniJS/AST.agda.bak MiniJS/Parser.agda MiniJS/Parser.agda.bak MiniJS/IOLib.agda MiniJS/IOLib.agda.bak MiniJS/Main.agda MiniJS/Main.agda.bak MiniJS/MiniJS.dtd MiniJS/MiniJS.dtd.bak MiniJS/Test MiniJS/Lex.hs MiniJS/Par.hs MiniJS/Par.info MiniJS/ParData.hs
	-rmdir -p MiniJS/
	-rm -rf Code/

# EOF
