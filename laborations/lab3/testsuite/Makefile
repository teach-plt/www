# John, 2015-11-04 Makefile for lab3 testsuite

# Files which contribute to the generated .html
deps=../../../style.css ../../../include.html ../../../enhance_page.js

.PHONY : all suite www

all : suite www

suite : lab3-testsuite.tar.gz

lab3-testsuite.tar.gz : build-tarball.sh progs-test-lab3.hs Makefile-test Runtime.java jasmin.jar \
                        good/*.cc good/*.cc.*put dir-for-path-test/one-more-dir/simple.cc
	./build-tarball.sh

www : index.html

%.html : %.txt $(deps)
	txt2tags --style=../../../style.css -t html $<

clean :
	rm -f index.html

# EOF
