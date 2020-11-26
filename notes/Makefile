.PHONY: all
all: index.html

files = type-checking.html interpreter.html monads.html compilation.html

index.html : $(files)
	tree -H '.' -L 2 --noreport --charset utf-8 > $@
# -L <depth>

%.html : %.md Makefile
	pandoc -f gfm -t html -o $@ -s $<

# EOF
