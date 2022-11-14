.PHONY: all
all: index.html

files = type-checking.html interpreter.html monads.html compilation.html cbn-cbv.html typing.html LR-table.html

index.html : $(files)
	tree -H '.' -L 2 --noreport --charset utf-8 > $@
# -L <depth>

%.html : %.md Makefile
	pandoc --css pandoc.css -f gfm -t html -o $@ --standalone $<

# visualize-cbn

.PHONY: vis-text vis-js
vis-text:
	visualize-cbn -i twice-double.hs --show-trace --hide-prelude

vis-js: twice-double.js

%.js : %.hs
	visualize-cbn -i $< --javascript $@ --hide-prelude

# EOF
