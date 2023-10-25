# Andreas, 2019-11-07 Makefile for mini

# Files which contribute to the generated .html
deps=../../style.css ../../enhance_page.js

.PHONY : all www

all : www miniJS.zip

miniJS.zip : src
	zip -q -r $@ src

www : index.html
	make -C src www

%.html : %.txt $(deps)
	txt2tags --style=../../style.css -t html $<


# EOF
