# Andreas, 2020-11-03 Makefile for laborations
#
# Distribute goals to the lab% subdirectories

## Build web pages

.PHONY : www www%

www : www1 www2 www3 www4

www% :
	make -C lab$* www

## Test

include mk/test.mk

.PHONY : test test%
test : test-build test1 test2 test3 test4

# Test compilation of JavaLabRunner
test-% : JavaLabRunner.hs
	ghc-$* -fno-code $<
	touch $@

# Test recursively in lab% directories
test% :
	make -C lab$* test

# EOF
