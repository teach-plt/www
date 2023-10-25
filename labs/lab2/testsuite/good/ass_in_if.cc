void nop() {
        // do nothing
}

int main() {
        int x = 5;
        if (true) x = 7; else nop();
        printInt(x);
        return 0;
}
