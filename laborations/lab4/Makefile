# John, 2015-11-04 Makefile for lab4

# Files which contribute to the generated .html
deps=../../style.css ../../include.html ../../enhance_page.js

.PHONY : all www testsuite

all : www testsuite lab4-stub-java.tar.gz lab4-stub-haskell.tar.gz

testsuite :
	make -C testsuite

lab4-stub-java.tar.gz : \
  java/Fun.cf \
  java/Makefile \
  java/lab4 \
  java/lab4.java \
  java/Interpreter.java
	tar czhf $@ $^

lab4-stub-haskell.tar.gz : \
  haskell/Fun.cf \
  haskell/Makefile \
  haskell/lab4.hs \
  haskell/Interpreter.hs
	tar czhf $@ $^

www : lab4.html

%.html : %.txt $(deps)
	txt2tags --style=../../style.css -t html $<


clean : clean_index clean_testsuite

clean_index :
	rm -f lab4.html

clean_testsuite :
	make clean -C testsuite/

# EOF
