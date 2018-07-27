# Makefile for www structure of PLT
.PHONY: all slides course-exercises

all : index.html slides course-exercises

slides :
	make -C slides

course-exercises :
	make -C course-exercises

%.html : %.txt
	txt2tags  -t html $<

# EOF
