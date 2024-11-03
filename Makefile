# Makefile for www structure of PLT

# Files which contribute to index.html
deps=style.css gh-fork-ribbon.css enhance_page.js Makefile

.PHONY : all ship lab% mini ipl-book notes labs-www www

all : exams mini lab1 lab2 lab3 lab4 ipl-book notes labs-www www # www last for linkchecker

# ship :
# 	ssh abela@remote12.chalmers.se -t 'bash -l -c up-plt.sh'
# #/chalmers/users/abela/bin/up-plt.sh

# ship : index.html
# #	scp $< frelindb@remote12.chalmers.se:/chalmers/groups/edu2009/www/www.cse.chalmers.se/year/2015/course/DAT151-lp2/
# 	scp $< abela@remote12.chalmers.se:/chalmers/groups/edu2009/www/www.cse.chalmers.se/year/2019/course/DAT151/

labs-www :
	make -C labs index.html

lab% :
	make -C labs/$@

mini :
	make -C labs/mini

ipl-book :
	make -C plt-book/ipl-book

notes :
	make -C notes
	make -C live

www : index.html

check : all
	linkchecker --check-extern index.html
# pip install git+https://github.com/linkchecker/linkchecker.git@v9.4.0

# # Note: this needs txt2tags version 2.6 (python2)
# # version 3.x produces different html (very different look)
# # and dumps a fixed CSS style into the generated html.
# index.html : %.html : %.txt $(deps)
# 	txt2tags --style=style.css --style=gh-fork-ribbon.css -t html $<
# # --toc;

# sed: Insert <br/> tags if line ends with punctuation.
# Do this via trailing spaces (markdown syntax for line break).
# (This then does no harm if inside code block.)
# Inside Makefile, need to use $$ for eol ($).
index.html : README.md Makefile pandoc.css
	sed -e 's#\([.,;:!?]\)$$#\1  #' $< | pandoc --css pandoc.css -f gfm -t html -o $@ --standalone
# --metadata title="Programming Language Technology"  ## This also adds a title to the rendering

## Exams

.PHONY: exams
exams : exams.tgz
	make -C exams

exams.tgz : exams/*.pdf exams/*.txt exams/*.html exams/*.jpg
	tar czf $@ $^

## Testing

.PHONY : test

test :
	make -C labs test

## Cleaning

clean : clean_index clean_lab1 clean_lab2 clean_lab3 clean_lab4 clean_mini

clean_index :
	rm -f index.html

clean_% :
	make clean -C labs/$*

# EOF
