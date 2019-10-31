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

www : index.html

%.html : %.txt $(deps)
	txt2tags --style=../../style.css -t html $<


clean : clean_index clean_stubs clean_testsuite

clean_index :
	rm -f index.html

clean_stubs :
	rm -f lab3-stub-haskell.tar.gz lab3-stub-java.tar.gz
	make -C haskell clean
	make -C java clean

clean_testsuite :
	make -C testsuite clean

# EOF
