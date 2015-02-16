int main () {
	int i = 1;
	printInt(i);
	true || i++ != 45;
	printInt(i);
	false || i++ >= 0;
	printInt(i);
	true && i++ < 0;
	printInt(i);
	false && i++ > 0;
	printInt(i);
	int j;
	if (34 < 6 && j < 0) {
		printInt(i);
	} else { 
		printInt (42); 
	}
}
