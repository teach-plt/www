.PHONY: all
all: index.html

files = exam-solution-2020-1.html

index.html : $(files)
	tree -H '.' -L 2 --noreport --charset utf-8 > $@
# -L <depth>

%.html : %.md %.yaml Makefile
	pandoc -s --metadata-file=$*.yaml -f gfm -t html -o $@ $<

# EOF
