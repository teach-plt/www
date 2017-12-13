grm = Fun
dir = $(grm)
bnfc_output = $(patsubst %,$(dir)/%,Abs.hs ErrM.hs Print.hs Lex.x Par.y Test.hs)
main-is = lab4.hs
executable = $(patsubst %.hs,%,$(main-is))
source_files = $(main-is) Interpreter.hs
object_files = $(patsubst %.hs,%.o,$(source_files)) $(patsubst %.hs,%.hi,$(source_files))
alex_file = $(dir)/Lex
happy_file = $(dir)/Par

HC_OPTS =

bnfc=$(HOME)/.cabal/bin/bnfc
BNFC_OPTS =
  # --java
  # --profile # DOES NOT WORK
  # --haskell-gadt

.PHONY: build

build : $(alex_file).hs $(happy_file).hs $(executable)

$(executable) : $(source_files) # $(alex_file).hs $(happy_file).hs
	ghc --make $(HC_OPTS) $(main-is) -o $(executable)


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
	rm -rf $(dir) GRM.txt $(executable) $(object_files)

# EOF
