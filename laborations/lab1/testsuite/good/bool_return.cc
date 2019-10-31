bool c (int d){
	return d < d;
}
int main () {
	if (c(0)) printInt(1);
	else printInt (0);
	return 0;
}
