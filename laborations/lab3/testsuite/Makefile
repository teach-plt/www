# John, 2015-11-04 Makefile for lab3 testsuite

.PHONY : all suite www

all : suite www

suite : lab3-testsuite.tar.gz

lab3-testsuite.tar.gz : build-tarball.sh progs-test-lab3.hs Makefile-test Runtime.java jasmin.jar index.txt good/*.cc good/*.cc.*put
	./build-tarball.sh

www : index.html

%.html : %.txt
	txt2tags --style=../../../style.css -t html $<

# EOF
