# Goals to build the test program

test : Runtime.class progs-test-lab3.hs
	ghc -threaded --make -o progs-test-lab3 progs-test-lab3.hs

Runtime.class : Runtime.java
	javac Runtime.java

clean :
	rm -f *.j good*.class core*.class
	rm -f good/*.j good/*.class


# Goals to build the web page

build-www : www suite
#	@echo "*****************************************************"
#	@echo "*                                                   *"
#	@echo "*  NOTE: To build the test program, please invoke   *"
#	@echo "*                                                   *"
#	@echo "*      make test                                    *"
#	@echo "*                                                   *"
#	@echo "*****************************************************"

suite : lab3-testsuite.tar.gz

lab3-testsuite.tar.gz : build-tarball.sh progs-test-lab3.hs Makefile Runtime.java jasmin.jar index.txt good/*.cc good/*.cc.*put
	./build-tarball.sh

www : index.html

%.html : %.txt
	txt2tags $<


# EOF
