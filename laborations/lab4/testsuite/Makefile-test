.PHONY : all

HC_OPTS =
PROG    = progs-test-lab4

all : $(PROG)

$(PROG) : $(PROG).hs
	ghc --make $(HC_OPTS) $< -o $@

clean :
	rm -f $(PROG).hi $(PROG).o $(PROG)
