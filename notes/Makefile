.PHONY: all
all: index.html

files = type-checking.html interpreter.html monads.html compilation.html cbn-cbv.html typing.html LR-table.html git-primer.html java.html

index.html : $(files)
	tree -H '.' -L 2 --noreport --charset utf-8 > $@
# -L <depth>

# sed: Insert <br/> tags if line ends with punctuation.
# Do this via trailing spaces (markdown syntax for line break).
# (This then does no harm if inside code block.)
# Inside Makefile, need to use $$ for eol ($).
%.html : %.md Makefile ../pandoc.css
	sed -e 's#\([.,;:!?]\)$$#\1  #' $< | pandoc --css ../pandoc.css -f gfm -t html -o $@ --standalone
# --metadata title="$*"

# visualize-cbn

.PHONY: vis-text vis-js
vis-text:
	visualize-cbn -i twice-double.hs --show-trace --hide-prelude

vis-js: twice-double.js

%.js : %.hs
	visualize-cbn -i $< --javascript $@ --hide-prelude

# debug

br-test:
	sed -e 's#\([.,!?]\)$$#\1<br/>#'

# EOF
