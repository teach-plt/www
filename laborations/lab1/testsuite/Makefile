# Andreas, 2014-01-19 Makefile for lab1 testsuite

# Files which contribute to the generated .html
deps=../../../style.css ../../../include.html ../../../enhance_page.js

.PHONY : all suite www

all : suite www

suite : lab1-testsuite.tar.gz

lab1-testsuite.tar.gz : build-tarball.sh progs-test-lab1.hs Makefile-test good/*.cc bad/*.cc
	./build-tarball.sh

www : index.html

%.html : %.txt $(deps)
	txt2tags --style=../../../style.css -t html $<

clean :
	rm -f index.html

# EOF
