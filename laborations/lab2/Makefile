# Andreas, 2014-02-09 Makefile for lab2

# Files which contribute to the generated .html
deps=../../style.css ../../include.html ../../enhance_page.js

.PHONY : all www testsuite

all : www testsuite lab2-stub-java.tar.gz lab2-stub-haskell.tar.gz

testsuite :
	make -C testsuite

lab2-stub-java.tar.gz : \
  java/CPP.cf \
  java/Makefile \
  java/lab2 \
  java/lab2.java \
  java/Interpreter.java \
  java/TypeChecker.java \
  java/TypeException.java
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
