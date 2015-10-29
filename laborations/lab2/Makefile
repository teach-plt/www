# Andreas, 2014-02-09 Makefile for lab2

.PHONY : all www testsuite

all : www testsuite

testsuite :
	make -C testsuite

www : lab2.html

%.html : %.txt
	txt2tags --style=../../style.css -t html $<

# EOF
