# John, 2015-02-16 Makefile for lab3

.PHONY : all www testsuite

all : www testsuite

testsuite :
	make -C testsuite build-www

www : lab3.html

%.html : %.txt
	txt2tags --style=../../style.css -t html $<

# EOF
