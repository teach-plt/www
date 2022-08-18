# Test compilation of the stub under various GHC versions.

test-goals = \
  test-8.0.2  \
  test-8.2.2  \
  test-8.4.4  \
  test-8.6.5  \
  test-8.8.4  \
  test-8.10.7 \
  test-9.0.2  \
  test-9.2.4  \
  test-9.4.1  \
# end test

.PHONY: test-build
test-build : test-cabal $(test-goals)

.PHONY: test-cabal
test-cabal:
	cabal build

clean-tests :
	@find . -name "test-*" -empty -delete

# EOF
