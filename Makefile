.PHONY : ship all www lab1 

all : www lab1

ship : index.html
	scp $< abela@remote12.chalmers.se:/chalmers/groups/edu2009/www/www.cse.chalmers.se/year/2015/course/DAT151/

lab1 : 
	make -C laborations/lab1

www : index.html

%.html : %.txt
	txt2tags $<

# EOF
