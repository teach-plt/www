# John, 2015-02-20 Makefile for lab4

.PHONY : all www testsuite

all : www testsuite

testsuite : testsuite.tgz

testsuite.tgz : testsuite/*.fun testsuite/progs-test-lab4.sh
	tar czf $@ $^

www : lab4.html

%.html : %.txt
	txt2tags --style=../../style.css -t html $<

# EOF
