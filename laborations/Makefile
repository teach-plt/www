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

test : test1 test2 test3 test4

test% :
	make -C lab$* test

# EOF
