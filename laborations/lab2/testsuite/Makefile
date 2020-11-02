# Andreas, 2014-02-10 Makefile for lab2 testsuite

# Files which contribute to the generated .html
deps=../../../style.css ../../../include.html ../../../enhance_page.js

.PHONY : all suite www test

all : suite www

suite : lab2-testsuite.tar.gz

lab2-testsuite.tar.gz : build-tarball.sh progs-test-lab2.hs Makefile-test bad/*.cc bad-runtime/*.cc good/*.cc good/*.cc.*put
	./build-tarball.sh

www : index.html

%.html : %.txt $(deps)
	txt2tags --style=../../../style.css -t html $<

# Test compilation of the test-suite runner under various GHC versions.
# Needs at least 7.10.3 because of System.Directory.listDirectory.

test : \
  test-7.10.3 \
  test-8.0.2  \
  test-8.2.2  \
  test-8.4.4  \
  test-8.6.4  \
  test-8.8.4  \
  test-8.10.2 \
# end test
# NB: 8.6.5 would be the latest 8.6, but I (Andreas Abel) do not have it installed.

test-% : progs-test-lab2.hs
	ghc-$* -fno-code $<
	touch $@

clean :
	rm -f index.html

# EOF
