.PHONY : ship

ship : lab1.html
	scp $< abela@remote12.chalmers.se:/chalmers/groups/edu2009/www/www.cse.chalmers.se/year/2015/course/DAT151/laborations/lab1/

%.html : %.txt
	txt2tags $<

# EOF
