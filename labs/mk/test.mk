# Test compilation of the stub under various GHC versions.

test-goals =  \
  test-8.4.4  \
  test-8.6.5  \
  test-8.8.4  \
  test-8.10.7 \
  test-9.0.2  \
  test-9.2.8  \
  test-9.4.8  \
  test-9.6.3  \
  test-9.8.1  \

# end test-goals

.PHONY: test-build
test-build : test-cabal test-stack $(test-goals)

.PHONY: test-cabal
test-cabal:
	cabal build

.PHONY: test-stack
test-stack:
	stack build

clean-tests :
	@find . -name "test-*" -empty -delete

# EOF
