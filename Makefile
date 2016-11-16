# .PHONY : ship all www lab1 lab2

all : clean www lab1 lab2 lab3 lab4 mini

ship : index.html
#	scp $< frelindb@remote12.chalmers.se:/chalmers/groups/edu2009/www/www.cse.chalmers.se/year/2015/course/DAT151-lp2/
	scp $< abela@remote12.chalmers.se:/chalmers/groups/edu2009/www/www.cse.chalmers.se/year/2016/course/DAT151/

lab% :
	make -C laborations/$@

mini :
	make -C laborations/mini

www : index.html

%.html : %.txt style.css
	txt2tags --style=style.css -t html $<


clean : clean_index clean_lab1 clean_lab2 clean_lab3 clean_lab4 clean_mini

clean_index :
	rm -f index.html

clean_% :
	make clean -C laborations/$*

# EOF
