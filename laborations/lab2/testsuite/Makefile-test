.PHONY : all

HC_OPTS = -threaded

all : progs-test-lab2

progs-test-lab2 : progs-test-lab2.hs
	ghc --make $(HC_OPTS) $< -o $@
