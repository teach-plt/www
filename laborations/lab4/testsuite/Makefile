# John, 2015-11-04 Makefile for lab4 testsuite

# Files which contribute to the generated .html
deps=../../../style.css ../../../include.html ../../../enhance_page.js

.PHONY : all suite www

all : suite www

suite : lab4-testsuite.tar.gz

lab4-testsuite.tar.gz : build-tarball.sh bad/*.hs good/*.hs progs-test-lab4
	./build-tarball.sh

www : index.html

%.html : %.txt $(deps)
	txt2tags --style=../../../style.css -t html $<

clean :
	rm -f index.html

# EOF
