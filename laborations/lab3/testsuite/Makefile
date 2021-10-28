# John, 2015-11-04 Makefile for lab3 testsuite

# Files which contribute to the generated .html
deps=../../../style.css ../../../include.html ../../../enhance_page.js

.PHONY : all suite www test

all : suite www

suite : lab3-testsuite.tar.gz

lab3-testsuite.tar.gz : build-tarball.sh progs-test-lab3.hs Makefile-test Runtime.java \
                        good/**/*.cc good/**/*.cc.*put dir-for-path-test/one-more-dir/simple.cc
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
  test-8.6.5  \
  test-8.8.4  \
  test-8.10.7 \
  test-9.0.1  \
# end test

test-% : progs-test-lab3.hs
	ghc-$* -fno-code $<
	touch $@

clean :
	rm -f index.html

# EOF
