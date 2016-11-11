# Andreas, 2014-02-09 Makefile for lab2

.PHONY : all www testsuite

all : www testsuite lab2-stub-java1.5.tar.gz

testsuite :
	make -C testsuite

lab2-stub-java1.5.tar.gz : \
  java1.5/CPP.cf \
  java1.5/Makefile \
  java1.5/lab2 \
  java1.5/lab2.java \
  java1.5/Interpreter.java \
  java1.5/TypeChecker.java \
  java1.5/TypeException.java
	tar czhf $@ $^

www : lab2.html

%.html : %.txt
	txt2tags --style=../../style.css -t html $<

# EOF
