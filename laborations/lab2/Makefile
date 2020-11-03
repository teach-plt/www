# Andreas, 2014-02-09 Makefile for lab2

# Files which contribute to the generated .html
deps=../../style.css ../../include.html ../../enhance_page.js

.PHONY : all www testsuite java haskell

all : www testsuite java haskell lab2-stub-java.tar.gz lab2-stub-haskell.tar.gz

testsuite :
	make -C testsuite

java haskell :
	make -C $@ sdist

lab2-stub-java.tar.gz : java/lab2.tar.gz
	cp $< $@

lab2-stub-haskell.tar.gz : haskell/lab2.tar.gz
	cp $< $@

www : index.html

%.html : %.txt $(deps)
	txt2tags --style=../../style.css -t html $<

## Testing

.PHONY : test

test :
	make -C testsuite test
	make -C haskell -f test.mk

## Cleaning

clean : clean_index clean_testsuite

clean_index :
	rm -f index.html

clean_testsuite :
	make clean -C testsuite/

# EOF
