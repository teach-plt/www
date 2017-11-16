.PHONY : all

HC_OPTS = -threaded

all : progs-test-lab3 Runtime.class

progs-test-lab3 : progs-test-lab3.hs
	ghc --make $(HC_OPTS) $< -o $@

Runtime.class : Runtime.java
	javac Runtime.java

clean :
	rm -f *.j good*.class core*.class
	rm -f good/*.j good/*.class
	rm -f dir-for-path-test/one-more-dir/*.j dir-for-path-test/one-more-dir/*.class
