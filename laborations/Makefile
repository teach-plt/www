# Andreas, 2020-11-03 Makefile for laborations
#
# Distribute goals to the lab% subdirectories

## Build web pages

.PHONY : www www%

www : www1 www2 www3 www4

www% :
	make -C lab$* www

## Test

.PHONY : test test%

test : test1 test2 test3 test4 \
  test-7.10.3 \
  test-8.0.2  \
  test-8.2.2  \
  test-8.4.4  \
  test-8.6.4  \
  test-8.8.4  \
  test-8.10.2 \
# end test
# NB: 8.6.5 would be the latest 8.6, but I (Andreas Abel) do not have it installed.

# Test compilation of JavaLabRunner
test-% : JavaLabRunner.hs
	ghc-$* -fno-code $<
	touch $@

# Test recursively in lab% directories
test% :
	make -C lab$* test

# EOF
