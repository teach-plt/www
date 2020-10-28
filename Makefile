# Makefile for www structure of PLT

# Files which contribute to index.html
deps=style.css include.html enhance_page.js Makefile

.PHONY : ship all www mini ipl-book
  # lab1 lab2 lab3 lab4 ## .PHONY turns off lab% goals somehow

all : exams.tgz mini lab1 lab2 lab3 lab4 ipl-book www # www last for linkchecker

ship : index.html
#	scp $< frelindb@remote12.chalmers.se:/chalmers/groups/edu2009/www/www.cse.chalmers.se/year/2015/course/DAT151-lp2/
	scp $< abela@remote12.chalmers.se:/chalmers/groups/edu2009/www/www.cse.chalmers.se/year/2019/course/DAT151/

lab% :
	make -C laborations/$@

mini :
	make -C laborations/mini

ipl-book :
	make -C plt-book/ipl-book

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

exams.tgz : exams/*.pdf exams/*.txt exams/*.html exams/*.jpg
	tar czf $@ $^

clean : clean_index clean_lab1 clean_lab2 clean_lab3 clean_lab4 clean_mini

clean_index :
	rm -f index.html

clean_% :
	make clean -C laborations/$*

# EOF
