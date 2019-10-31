.PHONY : all

HC_OPTS = -threaded

all : progs-test-lab1

progs-test-lab1 : progs-test-lab1.hs
	ghc --make $(HC_OPTS) -cpp -DHC_OPTS="\"$(HC_OPTS)\"" $< -o $@
