# John, 2015-02-16 Makefile for lab3

.PHONY : all www testsuite

all : www testsuite

testsuite :
	make -C testsuite

www : lab3.html

%.html : %.txt
	txt2tags --style=../../style.css -t html $<


clean : clean_index clean_testsuite

clean_index :
	rm -f lab3.html

clean_testsuite :
	make clean -C testsuite/

# EOF
