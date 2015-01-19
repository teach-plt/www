# Andreas, 2014-01-19 Makefile for lab1

.PHONY : all www testsuite

all : www testsuite

testsuite :
	make -C testscript

www : lab1.html

%.html : %.txt
	txt2tags $<

# EOF
