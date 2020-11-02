int main() {
	if (x1) x2; else x3;
	if (x1) { x2; } else x3;
	if (x1) x2; else { x3; }
	if (x1) { x2; } else { x3; }
}
