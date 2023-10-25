# John, 2015-11-04 Makefile for lab4 testsuite

# Files which contribute to the generated .html
deps=../../../style.css ../../../enhance_page.js

goodhs=$(wildcard good/*.hs)
goodgolden=$(patsubst %.hs,%.golden,$(goodhs))

.PHONY : all suite www test

all : good-haskell suite www

suite : lab4-testsuite.tar.gz

lab4-testsuite.tar.gz : build-tarball.sh plt-test-lab4.cabal plt-test-lab4.hs bad/*.hs $(goodhs)
	./build-tarball.sh

www : index.html

%.html : %.txt $(deps)
	txt2tags --style=../../../style.css -t html $<

# Test good tests for valid Haskell by running them.

good-haskell: $(goodgolden)

%.golden : %.hs
	runghc $< | tee $@

# Test compilation of the test-suite runner under various GHC versions.
# Needs at least 7.10.3 because of System.Directory.listDirectory.

include ../../mk/test.mk

test : test-build

test-% : plt-test-lab4.hs
	ghc-$* -fno-code $<
	@touch $@

clean :
	rm -f index.html $(goodgolden)

# EOF
