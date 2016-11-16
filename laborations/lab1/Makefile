# Andreas, 2014-01-19 Makefile for lab1

.PHONY : all www testsuite

all : www testsuite

testsuite :
	make -C testsuite

www : lab1.html

%.html : %.txt
	txt2tags --style=../../style.css -t html $<


clean : clean_index clean_testsuite

clean_index :
	rm -f lab1.html

clean_testsuite :
	make clean -C testsuite/

# EOF
