# John, 2015-02-20 Makefile for lab4

.PHONY : all www testsuite

all : www testsuite

testsuite :
	make -C testsuite build-www

www : lab4.html

%.html : %.txt
	txt2tags $<

# EOF
