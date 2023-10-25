int main() {
	if (x1) if (x2) x2; else x3;
	if (x1) if (x2) x2; else if (x3) x3;
	if (x1) if (x2) x2; else if (x3) x4; else x3;
	if (x1) { if (x2) x2; } else if (x3) x4; else x3;
	if (x1) { if (x2) x2; } else { if (x3) x4; else x3; }
	if (x1) { if (x2) x2; else if (x3) x4; else x3; }
}
