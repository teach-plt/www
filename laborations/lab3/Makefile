# John   , 2015-02-16 Makefile for lab3
# Andreas, 2018-12-03 Added stubs like for lab2

# Files which contribute to the generated .html
deps=../../style.css ../../include.html ../../enhance_page.js

.PHONY : all www testsuite java haskell

all : www testsuite java haskell lab3-stub-java.tar.gz lab3-stub-haskell.tar.gz

testsuite :
	make -C testsuite

java haskell :
	make -C $@ sdist

lab3-stub-java.tar.gz : java/lab3.tar.gz
	cp $< $@

lab3-stub-haskell.tar.gz : haskell/lab3.tar.gz
	cp $< $@

www : lab3.html

%.html : %.txt $(deps)
	txt2tags --style=../../style.css -t html $<


clean : clean_index clean_testsuite

clean_index :
	rm -f lab3.html

clean_testsuite :
	make clean -C testsuite/

# EOF
