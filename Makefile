# .PHONY : ship all www lab1 lab2

all : www lab1 lab2 lab3 lab4

ship : index.html
	scp $< frelindb@remote12.chalmers.se:/chalmers/groups/edu2009/www/www.cse.chalmers.se/year/2015/course/DAT151-lp2/
#	scp $< abela@remote12.chalmers.se:/chalmers/groups/edu2009/www/www.cse.chalmers.se/year/2015/course/DAT151/

lab% :
	make -C laborations/$@

www : index.html

%.html : %.txt
	txt2tags --style=style.css -t html $<

# EOF
