# John, 2015-11-04 Makefile for lab4 testsuite

.PHONY : all suite www

all : suite www

suite : lab4-testsuite.tar.gz

lab4-testsuite.tar.gz : *.fun progs-test-lab4.sh
	./build-tarball.sh

www : index.html

%.html : %.txt
	txt2tags --style=../../../style.css -t html $<

# EOF
