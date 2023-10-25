# Makefile for MiniJS example language

# List of goals not corresponding to file names.

.PHONY : all clean distclean test-parser www index.html

# Default goal.

all : MiniJS/Test test-parser

test-parser : MiniJS/Test
	MiniJS/Test test/ex1.js
	MiniJS/Test test/arith.js

# Create directory listing

www : index.html
	make -C haskell www
	make -C java www
	make -C test www

index.html :
	tree -H '.' -L 2 --noreport --charset utf-8 > $@
# -L <depth>

# Rules for building the parser.

MiniJS/ErrM.hs MiniJS/Lex.x MiniJS/Print.hs MiniJS/Par.y MiniJS/Test.hs : MiniJS.cf
	bnfc --haskell -d MiniJS.cf

%.hs : %.y
	happy --ghc --coerce --array --info $<

%.hs : %.x
	alex --ghc $<

MiniJS/Test : MiniJS/Test.hs MiniJS/ErrM.hs MiniJS/Lex.hs MiniJS/Par.hs MiniJS/Print.hs
	ghc --make $< -o $@

# Rules for cleaning generated files.

clean :
	-rm -f MiniJS/*.hi MiniJS/*.o MiniJS/*.log MiniJS/*.aux MiniJS/*.dvi

distclean : clean
	-rm -f MiniJS/Abs.hs MiniJS/Abs.hs.bak MiniJS/ComposOp.hs MiniJS/ComposOp.hs.bak MiniJS/Doc.txt MiniJS/Doc.txt.bak MiniJS/ErrM.hs MiniJS/ErrM.hs.bak MiniJS/Layout.hs MiniJS/Layout.hs.bak MiniJS/Lex.x MiniJS/Lex.x.bak MiniJS/Par.y MiniJS/Par.y.bak MiniJS/Print.hs MiniJS/Print.hs.bak MiniJS/SharedString.hs MiniJS/SharedString.hs.bak MiniJS/Skel.hs MiniJS/Skel.hs.bak MiniJS/Test.hs MiniJS/Test.hs.bak MiniJS/XML.hs MiniJS/XML.hs.bak MiniJS/AST.agda MiniJS/AST.agda.bak MiniJS/Parser.agda MiniJS/Parser.agda.bak MiniJS/IOLib.agda MiniJS/IOLib.agda.bak MiniJS/Main.agda MiniJS/Main.agda.bak MiniJS/MiniJS.dtd MiniJS/MiniJS.dtd.bak MiniJS/Test MiniJS/Lex.hs MiniJS/Par.hs MiniJS/Par.info MiniJS/ParData.hs Makefile
	-rmdir -p MiniJS/

# EOF
