int main() {
	printInt(fac(10));
	printInt(rfac(10));
	printInt(mfac(10));
        printInt(ifac(10));
        return 0 ;
}

int fac(int a) {
	int r;
	int n;

	r = 1;
	n = a;
	while (n > 0) {
		r = r * n;
		n = n - 1;
	}
	return r;
}

int rfac(int n)
{
  int f ;
	if (n == 0)
		f = 1;
	else
		f = n * rfac(n-1);
	return f ;
}

int mfac(int n)
{
  int f ;
	if (n == 0)
		f = 1;
	else
		f = n * nfac(n-1);
	return f ;
}

int nfac(int n)
{
  int f ;
	if (n != 0)
		f = mfac(n-1) * n;
	else
		f = 1;
	return f ;
}

int ifac(int n) { return ifac2f(1,n); }

int ifac2f(int l, int h) {
  int f ;
        if (l == h)
                f = l;
        else if (l > h)
                f = 1;
	else {
		int m;
		m = (l+h) / 2;
		f = ifac2f(l,m) * ifac2f(m+1,h);
	}
	return f ;
}
