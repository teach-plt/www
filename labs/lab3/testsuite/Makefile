# John, 2015-11-04 Makefile for lab3 testsuite

# Files which contribute to the generated .html
deps=../../../style.css ../../../enhance_page.js

.PHONY : all suite www test

all : suite www

suite : lab3-testsuite.tar.gz

lab3-testsuite.tar.gz : build-tarball.sh plt-test-lab3.cabal plt-test-lab3.hs Makefile-test Runtime.java \
                        good/**/*.cc good/**/*.cc.*put dir-for-path-test/one-more-dir/simple.cc
	./build-tarball.sh

www : index.html

%.html : %.txt $(deps)
	txt2tags --style=../../../style.css -t html $<

# Test compilation of the test-suite runner under various GHC versions.
# Needs at least 7.10.3 because of System.Directory.listDirectory.

include ../../mk/test.mk

test : test-build

test-% : plt-test-lab3.hs
	ghc-$* -fno-code $<
	@touch $@

clean :
	rm -f index.html

# EOF
