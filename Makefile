# Makefile for www structure of PLT

# Files which contribute to index.html
deps=style.css include.html enhance_page.js

.PHONY : ship all www mini ipl-book
  # lab1 lab2 lab3 lab4 ## .PHONY turns off lab% goals somehow

all : exams.tgz lab1 lab2 lab3 lab4 ipl-book www # www last for linkchecker

ship : index.html
#	scp $< frelindb@remote12.chalmers.se:/chalmers/groups/edu2009/www/www.cse.chalmers.se/year/2015/course/DAT151-lp2/
	scp $< abela@remote12.chalmers.se:/chalmers/groups/edu2009/www/www.cse.chalmers.se/year/2017/course/DAT151/

lab% :
	make -C laborations/$@

mini :
	make -C laborations/mini

ipl-book :
	make -C ipl-book

www : index.html

check : all
	linkchecker --check-extern index.html

%.html : %.txt $(deps)
	txt2tags --style=style.css -t html $<

exams.tgz : exams/*.pdf exams/*.txt exams/*.html exams/*.jpg
	tar czf $@ $^

clean : clean_index clean_lab1 clean_lab2 clean_lab3 clean_lab4 clean_mini

clean_index :
	rm -f index.html

clean_% :
	make clean -C laborations/$*

# EOF
