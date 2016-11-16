# John, 2015-11-04 Makefile for lab4

# Files which contribute to the generated .html
deps=../../style.css ../../include.html ../../enhance_page.js

.PHONY : all www testsuite

all : www testsuite

testsuite :
	make -C testsuite

www : lab4.html

%.html : %.txt $(deps)
	txt2tags --style=../../style.css -t html $<


clean : clean_index clean_testsuite

clean_index :
	rm -f lab4.html

clean_testsuite :
	make clean -C testsuite/

# EOF
