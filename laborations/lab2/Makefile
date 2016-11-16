# Andreas, 2014-02-09 Makefile for lab2

# Files which contribute to the generated .html
deps=../../style.css ../../include.html ../../enhance_page.js

.PHONY : all www testsuite

all : www testsuite lab2-stub-java1.5.tar.gz lab2-stub-haskell.tar.gz

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

lab2-stub-haskell.tar.gz : \
  haskell/CPP.cf \
  haskell/Makefile \
  haskell/lab2.hs \
  haskell/Interpreter.hs \
  haskell/TypeChecker.hs
	tar czhf $@ $^

www : lab2.html

%.html : %.txt $(deps)
	txt2tags --style=../../style.css -t html $<


clean : clean_index clean_testsuite

clean_index :
	rm -f lab2.html

clean_testsuite :
	make clean -C testsuite/

# EOF
