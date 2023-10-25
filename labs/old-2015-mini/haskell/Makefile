.PHONY: all bnfc runmini compilemini

all: compiler interpreter

compiler: bnfc compilemini
interpreter: bnfc runmini

compilemini:
	ghc --make compilemini.hs -o compilemini

runmini:
	ghc --make runmini.hs -o runmini

bnfc:
	bnfc --haskell ../Mini.cf
	happy -gca ParMini.y
	alex -g LexMini.x

clean:
	-rm -f runmini
	-rm -f *.log *.aux *.hi *.o *.dvi
	-rm -f DocMini.ps

distclean: clean
	-rm -f DocMini.* LexMini.* ParMini.* LayoutMini.* SkelMini.* PrintMini.* TestMini.* AbsMini.* TestMini ErrM.* SharedString.* Mini.dtd XMLMini.*

