.PHONY : all clean

HC_OPTS = -threaded

all : progs-test-lab2

progs-test-lab2 : progs-test-lab2.hs
	ghc --make $(HC_OPTS) $< -o $@

clean :
	rm -f progs-test-lab2

# EOF
