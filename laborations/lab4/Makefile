# John,    2015-11-04 Makefile for lab4
# Andreas, 2018-12-16 Added Haskell and Java stubs

# Files which contribute to the generated .html
deps=../../style.css ../../include.html ../../enhance_page.js

.PHONY : all www testsuite java haskell

all : www testsuite java haskell lab4-stub-java.tar.gz lab4-stub-haskell.tar.gz

testsuite :
	make -C testsuite

java haskell :
	make -C $@ sdist

lab4-stub-java.tar.gz : java/lab4.tar.gz
	cp $< $@

lab4-stub-haskell.tar.gz : haskell/lab4.tar.gz
	cp $< $@

www : lab4.html

%.html : %.txt $(deps)
	txt2tags --style=../../style.css -t html $<


clean : clean_index clean_testsuite

clean_index :
	rm -f lab4.html

clean_testsuite :
	make clean -C testsuite/

# EOF
