grm = Fun
dir = $(grm)
bnfc_output = $(patsubst %,$(dir)/%,Abs.hs ErrM.hs Print.hs Lex.x Par.y Test.hs)
alex_file = $(dir)/Lex
happy_file = $(dir)/Par
test_file = ./lab4

bnfc=$(HOME)/.cabal/bin/bnfc
BNFC_OPTS =
  # --java
  # --profile # DOES NOT WORK
  # --haskell-gadt

.PHONY: build test

build : $(alex_file).hs $(happy_file).hs $(test_file)

lab4 : lab4.hs Interpreter.hs # $(alex_file).hs $(happy_file).hs
	ghc --make lab4.hs -o lab4

test : $(test_file)
	$(test_file) test/good1.hs
	$(test_file) -n test/good1.hs
	$(test_file) -n test/good2.hs
	$(test_file) test/good3.hs
	$(test_file) -n test/good3.hs
	$(test_file) test/good4.hs
#	$(test_file) -n test/good4.hs
	$(test_file) -n test/good5.hs
	$(test_file) test/good6.hs
#	$(test_file) -n test/good6.hs
	$(test_file) test/good7.hs
#	$(test_file) -n test/good7.hs
	$(test_file) test/good8.hs
	$(test_file) -n test/good8.hs
	$(test_file) test/good9.hs
	$(test_file) -n test/good9.hs
	! $(test_file) test/bad1.hs
	! $(test_file) test/bad2.hs
	! $(test_file) test/bad3.hs
	! $(test_file) test/bad4.hs
	! $(test_file) test/bad5.hs
	! $(test_file) test/bad6.hs


$(bnfc_output): $(grm).cf
#	-@rm $(bnfc_output)
	$(bnfc) $(BNFC_OPTS) -d $<

$(alex_file).hs: $(alex_file).x
	alex --ghc $<

$(happy_file).hs: $(happy_file).y
	happy --ghc --coerce --array -iGRM.txt $<

.PHONY: clean debug

debug:
	@echo "bnfc_output = $(bnfc_output)"

clean:
	rm -rf bnfc

# EOF
