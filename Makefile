# Makefile for www structure of PLT

# Files which contribute to index.html
deps=style.css include.html enhance_page.js Makefile

.PHONY : all ship lab% mini ipl-book notes www

all : exams mini lab1 lab2 lab3 lab4 ipl-book notes www # www last for linkchecker

ship : all
	ssh abela@remote12.chalmers.se up-plt.sh

# ship : index.html
# #	scp $< frelindb@remote12.chalmers.se:/chalmers/groups/edu2009/www/www.cse.chalmers.se/year/2015/course/DAT151-lp2/
# 	scp $< abela@remote12.chalmers.se:/chalmers/groups/edu2009/www/www.cse.chalmers.se/year/2019/course/DAT151/

lab% :
	make -C laborations/$@

mini :
	make -C laborations/mini

ipl-book :
	make -C plt-book/ipl-book

notes :
	make -C notes
	make -C live

www : index.html

check : all
	linkchecker --check-extern index.html
# pip install git+https://github.com/linkchecker/linkchecker.git@v9.4.0

# Note: this needs txt2tags version 2.6 (python2)
# version 3.x produces different html (very different look)
# and dumps a fixed CSS style into the generated html.
index.html : %.html : %.txt $(deps)
	txt2tags --style=style.css -t html $<
# --toc

## Exams

.PHONY: exams
exams : exams.tgz
	make -C exams

exams.tgz : exams/*.pdf exams/*.txt exams/*.html exams/*.jpg
	tar czf $@ $^

## Testing

.PHONY : test

test :
	make -C laborations test

## Cleaning

clean : clean_index clean_lab1 clean_lab2 clean_lab3 clean_lab4 clean_mini

clean_index :
	rm -f index.html

clean_% :
	make clean -C laborations/$*

# EOF
