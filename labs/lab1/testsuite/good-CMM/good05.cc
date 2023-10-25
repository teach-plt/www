int main () {
	int lo,hi,mx ;

        lo = 1 ;
	hi = lo ;

        //mx = 5000000 ; //readInt () ;
        mx = readInt () ;

       	printInt(lo) ;
	while (hi < mx) {
	    printInt(hi) ;
	    hi = lo + hi ;
	    lo = hi - lo ;
	}

	return 0;
    }
