int main() {
	int x ;
	x = 6 ;
	int y ;
	y = x + 7 ;
	printInt(y);
	{
		int y ;
		y = 4 ;
		printInt(y);
		x = y ;
		printInt(x);
	}
	printInt(x);
	printInt(y);

	return 0;
}
