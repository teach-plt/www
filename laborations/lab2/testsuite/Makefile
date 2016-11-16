# Andreas, 2014-02-10 Makefile for lab2 testsuite

.PHONY : all suite www

all : suite www

suite : lab2-testsuite.tar.gz

lab2-testsuite.tar.gz : build-tarball.sh progs-test-lab2.hs Makefile-test bad/*.cc good/*.cc good/*.cc.*put
	./build-tarball.sh

www : index.html

%.html : %.txt
	txt2tags --style=../../../style.css -t html $<

clean :
	rm -f index.html

# EOF
