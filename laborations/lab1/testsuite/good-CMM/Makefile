# Makefile for Mandelbrot

default : fractal.pgm
	open $<

# fractal.pgm = magic header "P2" + output of mandelbrot.exe
fractal.pgm : mandelbrot.exe
	echo "P2" > $@
	./mandelbrot.exe >> $@

mandelbrot.exe : 3-mandelbrot.cc
	g++ -o$@ $<

# EOF
