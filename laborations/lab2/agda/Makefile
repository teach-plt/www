## Programming Language Technology (Chalmers DAT 151, GU DIT 231)
## (C) Andreas Abel, 2023
## All rights reserved.

# Makefile for lab2 in Agda.

AGDA       = agda
GHC        = ghc
HAPPY      = happy
HAPPY_OPTS = --array --info --ghc --coerce
ALEX       = alex
ALEX_OPTS  = --ghc

# Default goal.

lab2 : transpile lab2.cabal
	cabal install --installdir=.

lab2.cabal : package.yaml
	hpack

# Generating the .hs sources from the .agda sources

.PHONY : transpile
transpile: MAlonzo/Code/Qlab2.hs

MAlonzo/Code/Qlab2.hs : src/lab2.agda src/Library.agda src/TypedSyntax.agda src/TypeChecker.agda src/Interpreter.agda \
       CMM/AST.agda CMM/Parser.agda
	${AGDA} --ghc --ghc-dont-call-ghc $<

# Rules for building the parser.

CMM/Abs.hs CMM/Lex.x CMM/Par.y CMM/Print.hs CMM/Test.hs CMM/AST.agda CMM/Parser.agda CMM/IOLib.agda CMM/Main.agda : CMM.cf
	bnfc --haskell -d --agda CMM.cf

%.hs : %.y
	${HAPPY} ${HAPPY_OPTS} $<

%.hs : %.x
	${ALEX} ${ALEX_OPTS} $<

# Rules for cleaning generated files.

.PHONY: clean
clean :
	-rm -f CMM/*.hi CMM/*.o CMM/*.log CMM/*.aux CMM/*.dvi CMM/*.agdai
	-rm -rf _build dist-newstyle src/MAlonzo

.PHONY: distclean
distclean : clean
	-rm -f lab2.cabal
	-rm -f CMM/Abs.hs CMM/Abs.hs.bak CMM/ComposOp.hs CMM/ComposOp.hs.bak CMM/Doc.txt CMM/Doc.txt.bak CMM/ErrM.hs CMM/ErrM.hs.bak CMM/Layout.hs CMM/Layout.hs.bak CMM/Lex.x CMM/Lex.x.bak CMM/Par.y CMM/Par.y.bak CMM/Print.hs CMM/Print.hs.bak CMM/Skel.hs CMM/Skel.hs.bak CMM/Test.hs CMM/Test.hs.bak CMM/XML.hs CMM/XML.hs.bak CMM/AST.agda CMM/AST.agda.bak CMM/Parser.agda CMM/Parser.agda.bak CMM/IOLib.agda CMM/IOLib.agda.bak CMM/Main.agda CMM/Main.agda.bak CMM/CMM.dtd CMM/CMM.dtd.bak CMM/Test CMM/Lex.hs CMM/Par.hs CMM/Par.info CMM/ParData.hs
	-rmdir -p CMM/

# EOF
