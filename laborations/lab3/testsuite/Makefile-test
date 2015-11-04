.PHONY : all

all : progs-test-lab3 Runtime.class

progs-test-lab3 : progs-test-lab3.hs
	ghc -threaded --make -o progs-test-lab3 progs-test-lab3.hs

Runtime.class : Runtime.java
	javac Runtime.java

clean :
	rm -f *.j good*.class core*.class
	rm -f good/*.j good/*.class
