# Andreas, 2014-01-19 Makefile for lab1 testsuite

# Files which contribute to the generated .html
deps=../../../style.css ../../../enhance_page.js

.PHONY : all suite www test

all : suite www

suite : lab1-testsuite.tar.gz

lab1-testsuite.tar.gz : build-tarball.sh plt-test-lab1.cabal plt-test-lab1.hs Makefile-test good/*.cc bad/*.cc
	./build-tarball.sh

www : index.html

%.html : %.txt $(deps)
	txt2tags --style=../../../style.css -t html $<

# Test compilation of the test-suite runner under various GHC versions.
# Needs at least 7.10.3 because of System.Directory.listDirectory.

include ../../mk/test.mk

test : test-build

test-% : plt-test-lab1.hs
	ghc-$* -fno-code $<
	@touch $@

clean :
	rm -f index.html

# EOF
