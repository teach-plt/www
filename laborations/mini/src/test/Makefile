.PHONY : default www index.html

default :
	make -C ..
	node pi.js

# Create directory listing

www : index.html

index.html :
	tree -H '.' -L 2 --noreport --charset utf-8 > $@

# EOF
